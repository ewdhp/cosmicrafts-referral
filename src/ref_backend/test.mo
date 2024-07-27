import HashMap "mo:base/HashMap";
import Nat "mo:base/Nat";
import Principal "mo:base/Principal";
import Iter "mo:base/Iter";
import Array "mo:base/Array";
import Nat64 "mo:base/Nat64";
import Random "mo:base/Random";
import Blob "mo:base/Blob";
import Nat8 "mo:base/Nat8";
import Buffer "mo:base/Buffer";
import Float "mo:base/Float";
import Int64 "mo:base/Int64";
import Text "mo:base/Text";

actor Test {
  type UUID = Text;
  type TierID = Nat;

  type RefAccount = {
    playerID : Principal;
    refByUUID : UUID;
    uuid : UUID;
    tiers : [Tier];
    tokens : [Token];
    netWorth : Float;
  };

  type Tier = {
    id : TierID;
    title : Text;
    desc : Text;
    status : Text;
    token : Token;
    validate : shared () -> async Bool;
  };

  type Token = {
    title : Text;
    amount : Nat;
  };

  private stable var _tiers : [Tier] = [];
  private var tiers : Buffer.Buffer<Tier> = Buffer.fromArray<Tier>(_tiers);

  private stable var _accounts : [(Principal, RefAccount)] = [];
  private var accounts : HashMap.HashMap<Principal, RefAccount> = HashMap.fromIter(
    Iter.fromArray(_accounts),
    0,
    Principal.equal,
    Principal.hash,
  );

  private stable var _refTokens : [(Principal, [Token])] = [];
  private var refTokens : HashMap.HashMap<Principal, [Token]> = HashMap.fromIter(
    Iter.fromArray(_refTokens),
    0,
    Principal.equal,
    Principal.hash,
  );

  system func preupgrade() {
    _accounts := Iter.toArray(accounts.entries());
    _refTokens := Iter.toArray(refTokens.entries());
    _tiers := Buffer.toArray(tiers);

  };
  system func postupgrade() {
    accounts := HashMap.fromIter(
      Iter.fromArray(_accounts),
      0,
      Principal.equal,
      Principal.hash,
    );
    refTokens := HashMap.fromIter(
      Iter.fromArray(_refTokens),
      0,
      Principal.equal,
      Principal.hash,
    );

    tiers := Buffer.fromArray<Tier>(_tiers);
    _tiers := [];
    _refTokens := [];
  };

  public func calculateDynamicMultiplier(tokenCount : Nat) : async Float {
    let value : Nat64 = Nat64.fromNat(tokenCount);
    let intValue : Int64 = Int64.fromNat64(value);
    let floatValue : Float = Float.fromInt64(intValue);
    return 1.0 + floatValue * 0.1;
  };

  public func calculateNetWorth(tokens : [Token]) : async Float {
    var totalAmount : Nat = 0;
    for (token in tokens.vals()) {
      totalAmount += token.amount;
    };
    let tokenCount = Array.size(tokens);
    let value : Nat64 = Nat64.fromNat(totalAmount);
    let intValue : Int64 = Int64.fromNat64(value);
    let floatValue : Float = Float.fromInt64(intValue);
    let multiplier = await calculateDynamicMultiplier(tokenCount);
    return floatValue * multiplier;
  };

  public func getTopPlayers(page : Nat) : async [(Principal, RefAccount)] {
    let playersArray = Buffer.fromArray<(Principal, RefAccount)>(Iter.toArray(accounts.entries()));

    // Calculate net worth for each player
    for (i in Iter.range(0, playersArray.size() - 1)) {
      let (principal, account) = playersArray.get(i);
      let netWorth = await calculateNetWorth(account.tokens);
      playersArray.add((
        principal,
        {
          playerID = account.playerID;
          refByUUID = account.refByUUID;
          uuid = account.uuid;
          tiers = account.tiers;
          tokens = account.tokens;
          netWorth = netWorth;
        },
      ));
    };

    let sortedPlayers = Array.sort(
      Buffer.toArray<(Principal, RefAccount)>(playersArray),
      func(a : (Principal, RefAccount), b : (Principal, RefAccount)) : {
        #less;
        #equal;
        #greater;
      } {
        if (a.1.netWorth < b.1.netWorth) { #less } else if (a.1.netWorth > b.1.netWorth) {
          #greater;
        } else { #equal };
      },
    );

    let start = page * 10;
    let end = if (start + 10 > Array.size(sortedPlayers)) {
      Array.size(sortedPlayers);
    } else { start + 10 };
    let paginatedPlayers = Iter.toArray(Array.slice(sortedPlayers, start, end));

    return paginatedPlayers;
  };

  public func validateSignupTier() : async Bool {
    return true;
  };
  public func validateDiscordTier() : async Bool {
    return true;
  };
  public func validateTweeterTier() : async Bool {
    return true;
  };

  let signupToken : Token = {
    title = "Referral Signup token";
    amount = 5;
  };

  let signupTier : Tier = {
    id = 0;
    title = "Tier 1 signup";
    desc = "Signup and recieve 10 tokens for free";
    status = "Complete";
    token = { title = "Tier 1 signup token"; amount = 5 };
    validate = validateSignupTier;
  };
  tiers.add(signupTier);

  let discordTier : Tier = {
    id = 1;
    title = "Tier 2 Discord";
    desc = "Join Cosmicrafts Discord server and recieve 10 tokens for free";
    status = "Progress";
    token = { title = "Tier 2 Discord token"; amount = 10 };
    validate = validateDiscordTier;
  };
  tiers.add(discordTier);

  let tweeterTier : Tier = {
    id = 2;
    title = "Tier 3 Tweeter";
    desc = "Three Tweeter tags and recieve 20 tokens for free";
    status = "Progress";
    token = { title = "Tier 3 Tweeter token"; amount = 20 };
    validate = validateTweeterTier;
  };
  tiers.add(tweeterTier);

  public func getAlltiers() : async [Tier] {
    return Buffer.toArray(tiers);
  };

  public func getCurrentPlayerTier(playerId : Principal) : async ?Tier {
    let player = accounts.get(playerId);

    switch (player) {
      case (null) {
        return null; // Player not found
      };
      case (?player) {
        for (tier in player.tiers.vals()) {
          if (tier.status == "progress") {
            return ?tier; // Return the first tier with 'progress' status
          };
        };
        return null; // No tier with 'progress' status found
      };
    };
  };

  //let entries : [(Principal, [Token])] = Iter.toArray(refTokens.entries());
  private func claimReferralToken(code : UUID, token : Token) : async (Bool, Text) {
    let id = switch (await principalByUUID(code)) {
      case null { return (false, "Code not found") };
      case (?id) { id };
    };

    switch (accounts.get(id)) {
      case null { return (false, "Player principal not found.") };
      case (?account) {

        if (account.refByUUID == code) {
          return (false, "Error. Code already redeemed");
        };
        let size = (Array.size(account.tokens));
        if (size > 3) { return (false, "Reached max referral per player") };
        // let (result, _) = await mintTokensStandalone(id, token.amount);
        if (size > 0) {

          let tokens : [[Token]] = Iter.toArray(refTokens.vals());

          let updAcc : RefAccount = {
            playerID = account.playerID;
            refByUUID = account.refByUUID;
            uuid = account.uuid;
            tiers = account.tiers;
            tokens = Array.append(tokens[0], [token]);
            netWorth = account.netWorth;
          };
          refTokens.put(account.playerID, updAcc.tokens);
          _refTokens := Iter.toArray(refTokens.entries());
          accounts.put(account.playerID, updAcc);
          _accounts := Iter.toArray(accounts.entries());
          return (true, "Referral token added to account." # Nat.toText(size));
        } else {
          let updAcc : RefAccount = {
            playerID = account.playerID;
            refByUUID = account.refByUUID;
            uuid = account.uuid;
            tiers = account.tiers;
            tokens = [token];
            netWorth = account.netWorth;
          };
          refTokens.put(account.playerID, updAcc.tokens);
          _refTokens := Iter.toArray(refTokens.entries());
          accounts.put(account.playerID, updAcc);
          _accounts := Iter.toArray(accounts.entries());

          return (true, "First referral token added to account. " # Nat.toText(size));

        };
        return (false, "Mint token error.");
      };
    };
  };
  //implementar con caller
  public shared ({ caller }) func claimTierToken(id : Principal, tierID : Nat) : async (Bool, Text) {
    switch (accounts.get(id)) {
      case null { return (false, "Player principal not found.") };
      case (?account) {

        // let (result, _) = await mintTokensStandalone(id, token.amount);

        let updTiers = Array.tabulate<Tier>(
          Array.size(account.tiers),
          func(i : Nat) : Tier {
            if (i == tierID) {
              let updTier : Tier = {
                id = account.tiers[i].id;
                title = account.tiers[i].title;
                desc = account.tiers[i].desc;
                status = "Complete";
                token = account.tiers[i].token;
                validate = account.tiers[i].validate;
              };
              return updTier;
            } else {
              return account.tiers[i];
            };
          },
        );

        let updAcc : RefAccount = {
          playerID = account.playerID;
          refByUUID = account.refByUUID;
          uuid = account.uuid;
          tiers = updTiers;
          tokens = account.tokens;
          netWorth = account.netWorth;
        };
        accounts.put(id, updAcc);
        _accounts := Iter.toArray(accounts.entries());
        return (true, "Tier completed, token minted");
      };
    };
  };

  public shared ({ caller }) func enrollPlayer(signupCode : ?Text) : async (Bool, Text) {
    switch (accounts.get(caller)) {
      case null {

        var code : Text = switch (signupCode) {
          case (null) { "" };
          case (?value) { value };
        };

        var id = await principalByUUID(code);

        switch (id) {
          case (null) {
            accounts.put(
              caller,
              {
                playerID = caller;
                refByUUID = "";
                uuid = await generateUUID64();
                tiers = await getAlltiers();
                tokens = [];
                netWorth = 0.0;
              },
            );
            _accounts := Iter.toArray(accounts.entries());
            let text = "Referral code not provided or code not found";
            return (true, "Player enrrolled with Tier 1 signup." # " " # text);
          };
          case (?id) {
            if (caller == id) {
              return (false, "Error. Auto Referral");
            };

            let (result, text) = await claimReferralToken(code, signupToken);

            accounts.put(
              caller,
              {
                playerID = caller;
                refByUUID = if (result) { code } else { "" };
                uuid = await generateUUID64();
                tiers = await getAlltiers();
                tokens = [];
                netWorth = 0.0;
              },
            );
            _accounts := Iter.toArray(accounts.entries());
            return (true, "Player enrrolled with Tier 1 signup." # " " # text);
          };

        };
      };
      case (?_) {
        return (false, "Error. Account exists.");
      };
    };
  };
  public func enrollByPrincipal(signupCode : ?Text, principal : Principal) : async (Bool, Text) {
    switch (accounts.get(principal)) {
      case null {

        let code : Text = switch (signupCode) {
          case (null) { "" };
          case (?value) { value };
        };

        var id = await principalByUUID(code);

        switch (id) {
          case (null) {
            accounts.put(
              principal,
              {
                playerID = principal;
                refByUUID = "";
                uuid = await generateUUID64();
                tiers = await getAlltiers();
                tokens = [];
                netWorth = 0.0;
              },
            );
            _accounts := Iter.toArray(accounts.entries());
            let text = "Referral code not provided or code not found";
            return (true, "Congratulations you're now enrrolled." # text);
          };
          case (?id) {
            let (_, text) = await claimReferralToken(code, signupToken);
            accounts.put(
              principal,
              {
                playerID = principal;
                refByUUID = code;
                uuid = await generateUUID64();
                tiers = await getAlltiers();
                tokens = [];
                netWorth = 0.0;
              },
            );
            _accounts := Iter.toArray(accounts.entries());
            return (true, "Player enrrolled with Tier 1 signup." # " " # text);
          };
        };
      };
      case (?_) {
        return (false, "Error. Account exists.");
      };
    };
  };

  public query ({ caller }) func getAccount() : async ?RefAccount {
    return accounts.get(caller);
  };
  public query func getAllAccounts() : async [RefAccount] {
    return Iter.toArray(accounts.vals());
  };

  public query ({ caller }) func signupLinkShare() : async Text {
    let route = "https://cosmicrafts.com/signup_prom/";
    let err = "https://cosmicrafts.com/signup_prom/account_not_found";
    switch (accounts.get(caller)) {
      case (?refAccount) {
        let uuid : Text = refAccount.uuid;
        route # uuid;
      };
      case null err;
    };
  };

  public query func principalByUUID(uuid : UUID) : async ?Principal {
    let mappedIter = Iter.filter<(Principal, RefAccount)>(
      Iter.fromArray(_accounts),
      func(x : (Principal, RefAccount)) : Bool {
        let acc = x.1;
        if (acc.uuid == uuid) {
          return true;
        };
        return false;
      },
    );
    let data : ?(Principal, RefAccount) = mappedIter.next();
    switch (data) {
      case (null) { null };
      case (?(principal, _)) { ?principal };
    };
  };

  private func generateUUID64() : async Text {
    let randomBytes = await Random.blob();
    var uuid : Nat = 0;
    let byteArray = Blob.toArray(randomBytes);
    for (i in Iter.range(0, 7)) {
      uuid := Nat.add(
        Nat.bitshiftLeft(uuid, 8),
        Nat8.toNat(
          byteArray[i]
        ),
      );
    };
    uuid := uuid % 2147483647;
    return Nat.toText(uuid);
  };

  //TEST
  public func createAccount(playerID : Principal, refByUUID : Text, uuid : Text) : async () {
    let netWorth = await calculateNetWorth([]);
    let newAccount : RefAccount = {
      playerID = playerID;
      refByUUID = refByUUID;
      uuid = uuid;
      tiers = [];
      tokens = [];
      netWorth = netWorth;
    };
    accounts.put(playerID, newAccount);
  };

  public func claimTokens(playerID : Principal, token : Token) : async () {
    let accountOpt = accounts.get(playerID);
    switch (accountOpt) {
      case (null) {
        // Account does not exist
        return;
      };
      case (?account) {
        // Update account with new token
        let updatedTokens = Array.append(account.tokens, [token]);
        let netWorth = await calculateNetWorth(updatedTokens);
        let updatedAccount : RefAccount = {
          playerID = account.playerID;
          refByUUID = account.refByUUID;
          uuid = account.uuid;
          tiers = account.tiers;
          tokens = updatedTokens;
          netWorth = netWorth;
        };
        accounts.put(playerID, updatedAccount);
      };
    };
  };

  public func blobToByteArray(blob : Blob.Blob) : async [Nat8] {
    Blob.toArray(blob);
  };
  public func blobToHex(blob : Blob.Blob) : async Text {
    let hexChars = "0123456789abcdef";
    let hexArray = Text.toArray(hexChars);
    var hexString = "";
    var a = Blob.toArray(blob);
    for (byte in Iter.fromArray(a)) {
      let highNibble = (byte >> 4) & 0x0F;
      let lowNibble = byte & 0x0F;
      hexString #= Text.concat(Text.fromChar(hexArray[Nat8.toNat(highNibble)]), Text.fromChar(hexArray[Nat8.toNat(lowNibble)]));
    };
    return hexString;
  };

  public func generateRandomToken() : async Token {
    let randomBlob = await Random.blob();
    let byte = Random.byteFrom(randomBlob);
    let amount = (byte % 10) + 1; // Generates a number from 1 to 10
    let tokenTitleBlob = await Random.blob();
    let tokenTitle = await blobToHex(tokenTitleBlob);
    return {
      title = "Token" # tokenTitle;
      amount = Nat8.toNat(amount);
    };
  };
  public func generateRandomPrincipal() : async Principal {
    let randomBytes = await Random.blob();
    let randomArray = Blob.toArray(randomBytes);
    // Truncate or pad to 29 bytes if necessary
    let truncatedBytes = Array.tabulate<Nat8>(
      29,
      func(i : Nat) : Nat8 {
        if (i < Array.size(randomArray)) randomArray[i] else 0;
      },
    );
    return Principal.fromBlob(Blob.fromArray(truncatedBytes));
  };

  // Helper function to generate a random UUID
  public func generateRandomUUID() : async Text {
    let randomBytes = await Random.blob();
    return await blobToHex(randomBytes);
  };

  public func createAndPopulateAccounts() : async [(Principal, RefAccount)] {
    for (i in Iter.range(1, 2)) {
      let playerID = await generateRandomPrincipal();
      let refByUUID = await generateRandomUUID();
      let uuid = await generateRandomUUID();

      await createAccount(playerID, refByUUID, uuid);

      // Each player will claim between 1 to 10 tokens
      let randomBlob = await Random.blob();
      let byte = Random.byteFrom(randomBlob);
      let tokenCount = (byte % 10) + 1; // Generates a number from 1 to 10
      for (_ in Iter.range(1, Nat8.toNat(tokenCount))) {
        let token = await generateRandomToken();
        await claimTokens(playerID, token);
      };
    };

    // Test the getTopPlayers function
    let topPlayersPage1 = await getTopPlayers(0);
    return topPlayersPage1;
  };

};
