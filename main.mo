import Array "mo:base/Array";
import HashMap "mo:base/HashMap";
import Iter "mo:base/Iter";
import Nat "mo:base/Nat";
import Principal "mo:base/Principal";
import Text "mo:base/Text";
import Time "mo:base/Time";
import List "mo:base/List";
import Hash "mo:base/Hash";
import Option "mo:base/Option";
import Bool "mo:base/Bool";
import Result "mo:base/Result";
import Buffer "mo:base/Buffer";
import Debug "mo:base/Debug";
import Float "mo:base/Float";

persistent actor Insurance {
  // Types
  public type PolicyId = Nat;
  public type UserId = Principal;
  public type TokenSymbol = Text;
  public type ProductType = {
    #Phone;
    #Laptop;
    #Tablet;
    #Other: Text;
  };

  public type InsurancePolicy = {
    id: PolicyId;
    owner: Principal;
    productType: ProductType;
    productDetails: {
      name: Text;
      model: Text;
      purchaseDate: Int;
      purchasePrice: Float;
      serialNumber: Text;
    };
    coverage: {
      startDate: Int;
      endDate: Int;
      coverageAmount: Float;
      monthlyPremium: Float;
    };
    status: {
      #Active;
      #Expired;
      #Cancelled;
      #Claimed;
    };
    claims: [Claim];
    createdAt: Int;
  };

  public type Claim = {
    id: Nat;
    policyId: PolicyId;
    description: Text;
    damageType: {
      #Physical;
      #Theft;
      #Malfunction;
      #Other: Text;
    };
    claimAmount: Float;
    evidenceUrls: [Text];
    status: {
      #Submitted;
      #UnderReview;
      #Approved;
      #Rejected;
    };
    submissionDate: Int;
    resolutionDate: ?Int;
  };

  public type PolicyRequest = {
    productType: ProductType;
    productDetails: {
      name: Text;
      model: Text;
      purchaseDate: Int;
      purchasePrice: Float;
      serialNumber: Text;
    };
    desiredCoverage: Float;
  };

  public type ClaimRequest = {
    policyId: PolicyId;
    description: Text;
    damageType: {
      #Physical;
      #Theft;
      #Malfunction;
      #Other: Text;
    };
    claimAmount: Float;
    evidenceUrls: [Text];
  };
    id: DropId;
    name: Text;
    description: Text;
    tokenSymbol: TokenSymbol;
    network: NetworkId;
    totalSupply: Nat;
    price: ?Nat; // Optional price in network's native currency
    startTime: Int; // Timestamp in nanoseconds
    endTime: ?Int; // Optional end time
    websiteUrl: ?Text;
    imageUrl: ?Text;
    createdAt: Int;
    createdBy: Principal;
    isActive: Bool;
  };

  public type DropCreationRequest = {
    name: Text;
    description: Text;
    tokenSymbol: TokenSymbol;
    network: NetworkId;
    totalSupply: Nat;
    price: ?Nat;
    startTime: Int;
    endTime: ?Int;
    websiteUrl: ?Text;
    imageUrl: ?Text;
  };

  public type DropUpdateRequest = {
    name: ?Text;
    description: ?Text;
    price: ?Nat;
    startTime: ?Int;
    endTime: ?Int;
    websiteUrl: ?Text;
    imageUrl: ?Text;
    isActive: ?Bool;
  };

  public type NotificationPreference = {
    #all;
    #specific: [TokenSymbol];
    #specificNetworks: [NetworkId];
  };

  public type UserProfile = {
    userId: UserId;
    notificationPreference: NotificationPreference;
    emailNotifications: Bool;
    pushNotifications: Bool;
    email: ?Text;
    createdAt: Int;
    lastUpdated: Int;
  };

  public type UserProfileUpdateRequest = {
    notificationPreference: ?NotificationPreference;
    emailNotifications: ?Bool;
    pushNotifications: ?Bool;
    email: ?Text;
  };

  public type Notification = {
    id: Nat;
    userId: UserId;
    dropId: DropId;
    title: Text;
    message: Text;
    createdAt: Int;
    isRead: Bool;
  };

  public type NotificationResponse = {
    notifications: [Notification];
    totalUnread: Nat;
  };

  public type Error = {
    #NotFound;
    #AlreadyExists;
    #NotAuthorized;
    #InvalidInput;
    #SystemError;
  };

  // Custom hash function for Nat values
  private func natHash(n: Nat): Hash.Hash {
    Text.hash(Nat.toText(n))
  };

  // State variables
  private var nextPolicyId : Nat = 0;
  private var nextClaimId : Nat = 0;

  // Token configuration
  private let TOKEN_SYMBOL = "ICP";
  private let MIN_COVERAGE_RATIO : Float = 0.5;
  private let MAX_COVERAGE_RATIO : Float = 2.0;
  private let BASE_PREMIUM_RATE : Float = 0.02; // 2% monthly premium

  // Stable storage for upgrades
  private var policiesEntries : [(PolicyId, InsurancePolicy)] = [];
  private var userPoliciesEntries : [(UserId, [PolicyId])] = [];

  // In-memory storage
  private transient var policies = HashMap.HashMap<PolicyId, InsurancePolicy>(10, Nat.equal, natHash);
  private transient var userPolicies = HashMap.HashMap<UserId, List.List<PolicyId>>(10, Principal.equal, Principal.hash);

  // Initialize from stable storage during upgrades
  system func preupgrade() {
    policiesEntries := Iter.toArray(policies.entries());
    
    userPoliciesEntries := Array.map<(UserId, List.List<PolicyId>), (UserId, [PolicyId])>(
      Iter.toArray(userPolicies.entries()),
      func((p, l)) { (p, List.toArray(l)) }
    );
  };

  system func postupgrade() {
    for ((id, policy) in policiesEntries.vals()) {
      policies.put(id, policy);
    };
    policiesEntries := [];

    for ((userId, policyIds) in userPoliciesEntries.vals()) {
      userPolicies.put(userId, List.fromArray(policyIds));
    };
    userPoliciesEntries := [];
  };

  // Calculate monthly premium based on product type and coverage
  private func calculatePremium(productType: ProductType, purchasePrice: Float, desiredCoverage: Float) : Float {
    var riskMultiplier : Float = switch (productType) {
      case (#Phone) 1.2;
      case (#Laptop) 1.3;
      case (#Tablet) 1.1;
      case (#Other(_)) 1.5;
    };
    
    let coverageRatio = desiredCoverage / purchasePrice;
    if (coverageRatio < MIN_COVERAGE_RATIO or coverageRatio > MAX_COVERAGE_RATIO) {
      return 0.0; // Invalid coverage ratio
    };
    
    return purchasePrice * BASE_PREMIUM_RATE * riskMultiplier * coverageRatio;
  };

  // Create a new insurance policy
  public shared(msg) func createPolicy(request: PolicyRequest) : async Result.Result<PolicyId, Error> {
    let owner = msg.caller;
    let currentTime = Time.now();
    
    // Calculate monthly premium
    let monthlyPremium = calculatePremium(
      request.productType,
      request.productDetails.purchasePrice,
      request.desiredCoverage
    );
    
    if (monthlyPremium == 0.0) {
      return #err(#InvalidInput);
    };
    
    let policy : InsurancePolicy = {
      id = nextPolicyId;
      owner = owner;
      productType = request.productType;
      productDetails = request.productDetails;
      coverage = {
        startDate = currentTime;
        endDate = currentTime + 365 * 24 * 60 * 60 * 1000000000; // 1 year in nanoseconds
        coverageAmount = request.desiredCoverage;
        monthlyPremium = monthlyPremium;
      };
      status = #Active;
      claims = [];
      createdAt = currentTime;
    };
    
    policies.put(nextPolicyId, policy);
    
    // Add to user's policies
    let userPolicyList = switch (userPolicies.get(owner)) {
      case null { List.nil<PolicyId>() };
      case (?list) { list };
    };
    userPolicies.put(owner, List.push(nextPolicyId, userPolicyList));
    
    let policyId = nextPolicyId;
    nextPolicyId += 1;
    
    return #ok(policyId);
  };

  // Submit an insurance claim
  public shared(msg) func submitClaim(request: ClaimRequest) : async Result.Result<Nat, Error> {
    let caller = msg.caller;
    
    switch (policies.get(request.policyId)) {
      case null { return #err(#NotFound) };
      case (?policy) {
        if (policy.owner != caller) {
          return #err(#NotAuthorized);
        };
        
        if (policy.status != #Active) {
          return #err(#InvalidInput);
        };
        
        if (request.claimAmount > policy.coverage.coverageAmount) {
          return #err(#InvalidInput);
        };
        
        let claim : Claim = {
          id = nextClaimId;
          policyId = request.policyId;
          description = request.description;
          damageType = request.damageType;
          claimAmount = request.claimAmount;
          evidenceUrls = request.evidenceUrls;
          status = #Submitted;
          submissionDate = Time.now();
          resolutionDate = null;
        };
        
        let updatedPolicy : InsurancePolicy = {
          policy with
          claims = Array.append(policy.claims, [claim]);
        };
        
        policies.put(request.policyId, updatedPolicy);
        
        let claimId = nextClaimId;
        nextClaimId += 1;
        
        return #ok(claimId);
      };
    };
  };

  // Get user's policies
  public shared(msg) func getMyPolicies() : async [InsurancePolicy] {
    let caller = msg.caller;
    
    switch (userPolicies.get(caller)) {
      case null { [] };
      case (?policyIdsList) {
        let policyIds = List.toArray(policyIdsList);
        Array.mapFilter<PolicyId, InsurancePolicy>(
          policyIds,
          func(id) { policies.get(id) }
        );
      };
    };
  };

  // Get policy details
  public query func getPolicy(policyId: PolicyId) : async Result.Result<InsurancePolicy, Error> {
    switch (policies.get(policyId)) {
      case null { #err(#NotFound) };
      case (?policy) { #ok(policy) };
    };
  };

  // Process monthly premium payment
  public shared(msg) func payPremium(policyId: PolicyId) : async Result.Result<(), Error> {
    let caller = msg.caller;
    
    switch (policies.get(policyId)) {
      case null { return #err(#NotFound) };
      case (?policy) {
        if (policy.owner != caller) {
          return #err(#NotAuthorized);
        };
        
        if (policy.status != #Active) {
          return #err(#InvalidInput);
        };
        
        // Here you would integrate with the ICP ledger to process the payment
        // For now, we'll just simulate the payment
        
        return #ok();
      };
    };
  };

  // Renew policy
  public shared(msg) func renewPolicy(policyId: PolicyId) : async Result.Result<(), Error> {
    let caller = msg.caller;
    let currentTime = Time.now();
    
    switch (policies.get(policyId)) {
      case null { return #err(#NotFound) };
      case (?policy) {
        if (policy.owner != caller) {
          return #err(#NotAuthorized);
        };
        
        if (policy.status != #Active and policy.status != #Expired) {
          return #err(#InvalidInput);
        };
        
        let updatedPolicy : InsurancePolicy = {
          policy with
          coverage = {
            policy.coverage with
            startDate = currentTime;
            endDate = currentTime + 365 * 24 * 60 * 60 * 1000000000; // 1 year in nanoseconds
          };
          status = #Active;
        };
        
        policies.put(policyId, updatedPolicy);
        return #ok();
      };
    };
  };

  // Cancel policy
  public shared(msg) func cancelPolicy(policyId: PolicyId) : async Result.Result<(), Error> {
    let caller = msg.caller;
    
    switch (policies.get(policyId)) {
      case null { return #err(#NotFound) };
      case (?policy) {
        if (policy.owner != caller) {
          return #err(#NotAuthorized);
        };
        
        if (policy.status != #Active) {
          return #err(#InvalidInput);
        };
        
        let updatedPolicy : InsurancePolicy = {
          policy with
          status = #Cancelled;
        };
        
        policies.put(policyId, updatedPolicy);
        return #ok();
      };
    };
  };

  // Process claim
  public shared(msg) func processClaim(policyId: PolicyId, claimId: Nat, approved: Bool) : async Result.Result<(), Error> {
    // Note: In a production system, this would be restricted to admin/insurance staff
    let caller = msg.caller;
    
    switch (policies.get(policyId)) {
      case null { return #err(#NotFound) };
      case (?policy) {
        let claimIndex = Array.indexOf<Claim>(
          policy.claims,
          { id = claimId; } : Claim,
          func(a: Claim, b: Claim) : Bool { a.id == b.id }
        );
        
        switch (claimIndex) {
          case null { return #err(#NotFound) };
          case (?index) {
            let claim = policy.claims[index];
            if (claim.status != #Submitted and claim.status != #UnderReview) {
              return #err(#InvalidInput);
            };
            
            let updatedClaim : Claim = {
              claim with
              status = if (approved) #Approved else #Rejected;
              resolutionDate = ?Time.now();
            };
            
            let updatedClaims = Array.tabulate<Claim>(
              policy.claims.size(),
              func(i: Nat) : Claim {
                if (i == index) { updatedClaim } else { policy.claims[i] }
              }
            );
            
            let updatedPolicy : InsurancePolicy = {
              policy with
              claims = updatedClaims;
              status = if (approved) #Claimed else policy.status;
            };
            
            policies.put(policyId, updatedPolicy);
            return #ok();
          };
        };
      };
    };
  };
    let creator = msg.caller;
    
    let drop : TokenDrop = {
      id = nextDropId;
      name = request.name;
      description = request.description;
      tokenSymbol = request.tokenSymbol;
      network = request.network;
      totalSupply = request.totalSupply;
      price = request.price;
      startTime = request.startTime;
      endTime = request.endTime;
      websiteUrl = request.websiteUrl;
      imageUrl = request.imageUrl;
      createdAt = Time.now();
      createdBy = creator;
      isActive = true;
    };

    drops.put(nextDropId, drop);
    let dropId = nextDropId;
    nextDropId += 1;
    
    // Notify users who are subscribed to this token or network
    ignore _notifyInterestedUsers(drop);
    
    return #ok(dropId);
  };

  // Update an existing token drop
  public shared(msg) func updateDrop(dropId: DropId, request: DropUpdateRequest) : async Result.Result<(), Error> {
    let caller = msg.caller;
    
    switch (drops.get(dropId)) {
      case null { return #err(#NotFound) };
      case (?existingDrop) {
        if (existingDrop.createdBy != caller) {
          return #err(#NotAuthorized);
        };
        
        let updatedDrop : TokenDrop = {
          id = existingDrop.id;
          name = Option.get(request.name, existingDrop.name);
          description = Option.get(request.description, existingDrop.description);
          tokenSymbol = existingDrop.tokenSymbol;
          network = existingDrop.network;
          totalSupply = existingDrop.totalSupply;
          price = switch (request.price) {
            case null { existingDrop.price };
            case (?p) { ?p };
          };
          startTime = Option.get(request.startTime, existingDrop.startTime);
          endTime = switch (request.endTime) {
            case null { existingDrop.endTime };
            case (?e) { ?e };
          };
          websiteUrl = switch (request.websiteUrl) {
            case null { existingDrop.websiteUrl };
            case (?url) { ?url };
          };
          imageUrl = switch (request.imageUrl) {
            case null { existingDrop.imageUrl };
            case (?url) { ?url };
          };
          createdAt = existingDrop.createdAt;
          createdBy = existingDrop.createdBy;
          isActive = Option.get(request.isActive, existingDrop.isActive);
        };
        
        drops.put(dropId, updatedDrop);
        
        // If the drop was activated, notify interested users
        if (Option.get(request.isActive, false) and not existingDrop.isActive) {
          ignore _notifyInterestedUsers(updatedDrop);
        };
        
        return #ok();
      };
    };
  };

  // Get drop by ID
  public query func getDrop(dropId: DropId) : async Result.Result<TokenDrop, Error> {
    switch (drops.get(dropId)) {
      case null { #err(#NotFound) };
      case (?drop) { #ok(drop) };
    };
  };

  // Get all active drops
  public query func getActiveDrops() : async [TokenDrop] {
    let activeDrops = Iter.toArray(
      Iter.filter(
        drops.vals(),
        func(drop: TokenDrop) : Bool {
          drop.isActive and drop.startTime <= Time.now() and 
          (Option.isNull(drop.endTime) or Option.get(drop.endTime, 0) > Time.now())
        }
      )
    );
    
    return activeDrops;
  };

  // Get upcoming drops (not started yet)
  public query func getUpcomingDrops() : async [TokenDrop] {
    let upcomingDrops = Iter.toArray(
      Iter.filter(
        drops.vals(),
        func(drop: TokenDrop) : Bool {
          drop.isActive and drop.startTime > Time.now()
        }
      )
    );
    
    return upcomingDrops;
  };

  // Get drops by network
  public query func getDropsByNetwork(network: NetworkId) : async [TokenDrop] {
    let networkDrops = Iter.toArray(
      Iter.filter(
        drops.vals(),
        func(drop: TokenDrop) : Bool {
          drop.isActive and drop.network == network
        }
      )
    );
    
    return networkDrops;
  };

  // Get drops by token symbol
  public query func getDropsByToken(tokenSymbol: TokenSymbol) : async [TokenDrop] {
    let tokenDrops = Iter.toArray(
      Iter.filter(
        drops.vals(),
        func(drop: TokenDrop) : Bool {
          drop.isActive and drop.tokenSymbol == tokenSymbol
        }
      )
    );
    
    return tokenDrops;
  };

  // User profile management
  public shared(msg) func createOrUpdateProfile(request: UserProfileUpdateRequest) : async Result.Result<(), Error> {
    let userId = msg.caller;
    let currentTime = Time.now();
    
    let profile = switch (userProfiles.get(userId)) {
      case null {
        // Create new profile
        {
          userId = userId;
          notificationPreference = Option.get(request.notificationPreference, #all);
          emailNotifications = Option.get(request.emailNotifications, true);
          pushNotifications = Option.get(request.pushNotifications, true);
          email = request.email;
          createdAt = currentTime;
          lastUpdated = currentTime;
        }
      };
      case (?existingProfile) {
        // Update existing profile
        {
          userId = existingProfile.userId;
          notificationPreference = Option.get(request.notificationPreference, existingProfile.notificationPreference);
          emailNotifications = Option.get(request.emailNotifications, existingProfile.emailNotifications);
          pushNotifications = Option.get(request.pushNotifications, existingProfile.pushNotifications);
          email = switch (request.email) {
            case null { existingProfile.email };
            case (?e) { ?e };
          };
          createdAt = existingProfile.createdAt;
          lastUpdated = currentTime;
        }
      };
    };
    
    userProfiles.put(userId, profile);
    return #ok();
  };

  // Get user profile
  public shared(msg) func getMyProfile() : async Result.Result<UserProfile, Error> {
    let userId = msg.caller;
    
    switch (userProfiles.get(userId)) {
      case null { #err(#NotFound) };
      case (?profile) { #ok(profile) };
    };
  };

  // Subscribe to a drop
  public shared(msg) func subscribeToDrop(dropId: DropId) : async Result.Result<(), Error> {
    let userId = msg.caller;
    
    switch (drops.get(dropId)) {
      case null { return #err(#NotFound) };
      case (?drop) {
        // Add drop to user's subscriptions
        let userDropsList = switch (userSubscriptions.get(userId)) {
          case null { List.nil<DropId>() };
          case (?list) { list };
        };
        
        // Check if already subscribed
        let exists = List.some<DropId>(userDropsList, func(id) { id == dropId });
        
        if (not exists) {
          userSubscriptions.put(userId, List.push(dropId, userDropsList));
          
          // Create notification for subscription
          let notification : Notification = {
            id = nextNotificationId;
            userId = userId;
            dropId = dropId;
            title = "Subscription Confirmed";
            message = "You are now subscribed to " # drop.name # " token drop notifications.";
            createdAt = Time.now();
            isRead = false;
          };
          
          notifications.put(nextNotificationId, notification);
          nextNotificationId += 1;
        };
        
        return #ok();
      };
    };
  };

  // Unsubscribe from a drop
  public shared(msg) func unsubscribeFromDrop(dropId: DropId) : async Result.Result<(), Error> {
    let userId = msg.caller;
    
    switch (userSubscriptions.get(userId)) {
      case null { return #err(#NotFound) };
      case (?list) {
        let updatedList = List.filter<DropId>(list, func(id) { id != dropId });
        userSubscriptions.put(userId, updatedList);
        return #ok();
      };
    };
  };

  // Get user's subscribed drops
  public shared(msg) func getMySubscriptions() : async [TokenDrop] {
    let userId = msg.caller;
    
    switch (userSubscriptions.get(userId)) {
      case null { [] };
      case (?dropIdsList) {
        let dropIds = List.toArray(dropIdsList);
        Array.mapFilter<DropId, TokenDrop>(
          dropIds,
          func(id) { drops.get(id) }
        )
      };
    };
  };

  // Get user's notifications
  public shared(msg) func getMyNotifications(markAsRead: Bool) : async NotificationResponse {
    let userId = msg.caller;
    
    let userNotifications = Iter.toArray(
      Iter.filter(
        notifications.vals(),
        func(notification: Notification) : Bool {
          notification.userId == userId
        }
      )
    );
    
    // Count unread notifications
    var unreadCount = 0;
    for (notification in userNotifications.vals()) {
      if (not notification.isRead) {
        unreadCount += 1;
        
        // Mark as read if requested
        if (markAsRead) {
          let updatedNotification : Notification = {
            id = notification.id;
            userId = notification.userId;
            dropId = notification.dropId;
            title = notification.title;
            message = notification.message;
            createdAt = notification.createdAt;
            isRead = true;
          };
          
          notifications.put(notification.id, updatedNotification);
        };
      };
    };
    
    return {
      notifications = userNotifications;
      totalUnread = unreadCount;
    };
  };

  // Mark notification as read
  public shared(msg) func markNotificationAsRead(notificationId: Nat) : async Result.Result<(), Error> {
    let userId = msg.caller;
    
    switch (notifications.get(notificationId)) {
      case null { return #err(#NotFound) };
      case (?notification) {
        if (notification.userId != userId) {
          return #err(#NotAuthorized);
        };
        
        let updatedNotification : Notification = {
          id = notification.id;
          userId = notification.userId;
          dropId = notification.dropId;
          title = notification.title;
          message = notification.message;
          createdAt = notification.createdAt;
          isRead = true;
        };
        
        notifications.put(notificationId, updatedNotification);
        return #ok();
      };
    };
  };

  // Private helper function to notify interested users about a drop
  private func _notifyInterestedUsers(drop: TokenDrop) : async () {
    for ((userId, profile) in userProfiles.entries()) {
      var shouldNotify = false;
      
      // Check notification preferences
      switch (profile.notificationPreference) {
        case (#all) { shouldNotify := true; };
        case (#specific(tokens)) {
          shouldNotify := Array.find<TokenSymbol>(tokens, func(token) { token == drop.tokenSymbol }) != null;
        };
        case (#specificNetworks(networks)) {
          shouldNotify := Array.find<NetworkId>(networks, func(network) { network == drop.network }) != null;
        };
      };
      
      if (shouldNotify) {
        let notification : Notification = {
          id = nextNotificationId;
          userId = userId;
          dropId = drop.id;
          title = "New Token Drop: " # drop.name;
          message = "A new token drop for " # drop.tokenSymbol # " on " # drop.network # " is now available. Check it out!";
          createdAt = Time.now();
          isRead = false;
        };
        
        notifications.put(nextNotificationId, notification);
        nextNotificationId += 1;
        
        // In a real application, we would also send email or push notifications here
      };
    };
  };
}
