import HashMap "mo:base/HashMap";
import Nat "mo:base/Nat";
import Principal "mo:base/Principal";
import Iter "mo:base/Iter";
import Array "mo:base/Array";
import Nat64 "mo:base/Nat64";
import Int "mo:base/Int";
import Time "mo:base/Time";
import Random "mo:base/Random";
import Blob "mo:base/Blob";
import Nat8 "mo:base/Nat8";
import Buffer "mo:base/Buffer";
import Float "mo:base/Float";
import Int64 "mo:base/Int64";
import Text "mo:base/Text";

//importa los tipos del ICRC1
import TypesICRC1 "../icrc1/Types";

//importa el nuevo Utils, para desacople de funciones que no interactuen con variables estables declaradas en el actor
import Utils "Utils";

actor {

  type UUID = Text;
  type TierID = Nat;
  type Multiplier = Float;
  type Networth = Float;

  type F = () -> Bool;
  private var validateFunc : Buffer.Buffer<F> = Buffer.Buffer<F>(0);

  type RefAccount = {
    playerID : Principal;
    refByUUID : UUID;
    uuid : UUID;
    alias : Text; //not implemented
    tiers : [Tier];
    tokens : [Token];
  };
  type Tier = {
    id : TierID;
    title : Text;
    desc : Text;
    status : Text;
    token : Token;
  };
  type Token = {
    title : Text;
    amount : Nat;
  };

  type RefAccountView = {
    playerID : Principal;
    playerName : Text;
    currentTier : Tier;
    tierTokenSum : Nat;
    signupTokenSum : Nat;
    tokenTotal : Nat;
    multiplier : Float;
    netWorth : Float;
    topPlayers : [TopPLayersView];
    topPosition : Nat;
    topTokenAmount : Nat;
    singupLink : Text;
  };
  type TopPLayersView = {
    playerName : Text;
    tokenCount : Int;
    multiplier : Float;
    netWorth : Float;
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

  let signupToken : Token = {
    title = "Referral Signup token";
    amount = 5;
  };

  let missionTier : Tier = {
    id = 0;
    title = "Tier 1 Mission";
    desc = "Complete mission 1 and get 10 tokens free";
    status = "Progress";
    token = { title = "Tier 1 mission token"; amount = 10 };
  };
  tiers.add(missionTier);

  let discordTier : Tier = {
    id = 1;
    title = "Tier 2 Discord";
    desc = "Join Cosmicrafts Discord server and recieve 10 tokens for free";
    status = "Progress";
    token = { title = "Tier 2 Discord token"; amount = 10 };
  };
  tiers.add(discordTier);

  let tweeterTier : Tier = {
    id = 2;
    title = "Tier 3 Tweeter";
    desc = "Three Tweeter tags and recieve 20 tokens for free";
    status = "Progress";
    token = { title = "Tier 3 Tweeter token"; amount = 25 };
  };
  tiers.add(tweeterTier);

  private func validateMissionTier() : Bool {
    return true;
  };
  private func validateDiscordTier() : Bool {
    return true;
  };
  private func validateTweeterTier() : Bool {
    return true;
  };
  validateFunc.add(validateMissionTier);
  validateFunc.add(validateDiscordTier);
  validateFunc.add(validateTweeterTier);

  public query func getAlltiers() : async [Tier] {
    return Buffer.toArray(tiers);
  };

  private func getCurrentPlayerTier(playerId : Principal) : async ?Tier {
    let player = accounts.get(playerId);
    switch (player) {
      case (null) {
        return null;
      };
      case (?player) {
        for (tier in player.tiers.vals()) {
          if (tier.status == "Progress") {
            return ?tier;
          };
        };
        let size = player.tiers.size();
        ?player.tiers.get(size - 1);
      };
    };
  };
  //this must be shared ({ caller })
  public func getRefaccountView(id : Principal) : async ?RefAccountView {
    let account = switch (accounts.get(id)) {
      case null { return null };
      case (?acc) {
        acc;
      };
    };
    let (multiplier, networth) = await getTokenomics(account);
    let currentTier = switch (await getCurrentPlayerTier(id)) {
      case null { return null };
      case (?tier) tier;
    };
    let r : RefAccountView = {
      playerID = id;
      playerName = account.alias;
      currentTier = currentTier;
      tierTokenSum = await getTierTokenSum(account);
      signupTokenSum = await getRefTokenSum(account);
      tokenTotal = await getTotalTokenSum(account);
      multiplier = multiplier;
      netWorth = networth;
      topPlayers = await getTopPlayers(0);
      topPosition = await getPlayerTopPosition(id);
      topTokenAmount = await getTopWeeklyTokenAmount(id);
      singupLink = await signupLinkShare(id);
    };
    ?r;
  };

  private func getPlayerTopPosition(id : Principal) : async Nat {
    0; //not implemented
  };

  public query func claimTopWeeklyToken(id : Principal) : async (Bool, Text) {
    (false, "not implemented");
  };

  private func getTopWeeklyTokenAmount(id : Principal) : async Nat {
    0; //not implemented
  };

  private func getTokenomics(account : RefAccount) : async (Multiplier, Networth) {
    let sum : Nat64 = Nat64.fromNat(await getTotalTokenSum(account));
    let intSum : Int64 = Int64.fromNat64(sum);
    let floatSum : Float = Float.fromInt64(intSum);
    (1.0 * floatSum * 0.1, 1.0 * (1.0 * floatSum * 0.1));
  };

  private func getTotalTokenSum(account : RefAccount) : async Nat {
    (await getRefTokenSum(account)) + (await getTierTokenSum(account));
  };

  private func getRefTokenSum(account : RefAccount) : async Nat {
    let tokenSum = Array.foldLeft<Token, Nat>(
      account.tokens,
      0,
      func(acc, token) {
        acc + token.amount;
      },
    );
    tokenSum;
  };

  private func getTierTokenSum(account : RefAccount) : async Nat {
    let tierTokenSum = Array.foldLeft<Tier, Nat>(
      account.tiers,
      0,
      func(acc, tier) {
        if (tier.status == "Complete") {
          acc + tier.token.amount;
        } else {
          acc;
        };
      },
    );
    tierTokenSum;
  };

  private func getTopPlayers(page : Nat) : async [TopPLayersView] {
    let playersArray = Buffer.fromArray<(Principal, RefAccount)>(
      Iter.toArray(accounts.entries())
    );
    var playersWithTokenSums : [(Principal, RefAccount, Nat)] = [];
    for (i in Iter.range(0, playersArray.size() - 1)) {
      let (principal, account) = playersArray.get(i);
      let (multiplier, networth) = await getTokenomics(account);
      let tokenSum = await getTotalTokenSum(account);
      playersWithTokenSums := Array.append(
        playersWithTokenSums,
        [(
          principal,
          {
            playerID = account.playerID;
            refByUUID = account.refByUUID;
            uuid = account.uuid;
            alias = account.alias;
            tiers = account.tiers;
            tokens = account.tokens;
            netWorth = networth;
            multiplier = multiplier;
          },
          tokenSum,
        )],
      );
    };
    let sortedPlayers = Array.sort(
      playersWithTokenSums,
      func(
        a : (Principal, RefAccount, Nat),
        b : (Principal, RefAccount, Nat),
      ) : {
        #less;
        #equal;
        #greater;
      } {
        if (a.2 > b.2) {
          #less;
        } else if (a.2 < b.2) {
          #greater;
        } else {
          #equal;
        };
      },
    );
    let start = page * 10;
    let end = if (start + 10 > Array.size(sortedPlayers)) {
      Array.size(sortedPlayers);
    } else { start + 10 };
    let paginatedPlayers = Iter.toArray(
      Array.slice(
        sortedPlayers,
        start,
        end,
      )
    );
    var viewArray : [TopPLayersView] = [];

    for ((_, refAccount, tokenSum) in paginatedPlayers.vals()) {
      let (multiplier, networth) = await getTokenomics(refAccount);
      let rowView : TopPLayersView = {
        playerName = refAccount.alias;
        tokenCount = tokenSum;
        multiplier = multiplier;
        netWorth = networth;
      };
      viewArray := Array.append(viewArray, [rowView]);
    };
    return viewArray;
  };

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
        if (size > 0) {
          let (minted, result) = await mintTokensStandalone(id, signupToken.amount);
          if (minted) {
            let tokens : [[Token]] = Iter.toArray(refTokens.vals());
            let updAcc : RefAccount = {
              playerID = account.playerID;
              refByUUID = account.refByUUID;
              uuid = account.uuid;
              alias = account.alias;
              tiers = account.tiers;
              tokens = Array.append(tokens[0], [token]);
            };
            refTokens.put(account.playerID, updAcc.tokens);
            _refTokens := Iter.toArray(refTokens.entries());
            accounts.put(account.playerID, updAcc);
            _accounts := Iter.toArray(accounts.entries());
            return (true, "Referral token added to account." # Nat.toText(size));

          } else {
            return (false, "Error miniting tokens." # result);
          };

        } else {

          let (minted, result) = await mintTokensStandalone(id, signupToken.amount);
          if (minted) {
            let updAcc : RefAccount = {
              playerID = account.playerID;
              refByUUID = account.refByUUID;
              uuid = account.uuid;
              alias = account.alias;
              tiers = account.tiers;
              tokens = [token];
            };
            refTokens.put(account.playerID, updAcc.tokens);
            _refTokens := Iter.toArray(refTokens.entries());
            accounts.put(account.playerID, updAcc);
            _accounts := Iter.toArray(accounts.entries());

            return (true, "First referral token added to account. " # Nat.toText(size));
          } else {
            return (false, "Error miniting tokens." # result);
          };

        };
        return (false, "Mint token error.");
      };
    };
  };
  //this must be with shared({caller})
  public func claimTierToken(id : Principal) : async (Bool, Text) {
    let (tierStatus, tierID) = switch (await getCurrentPlayerTier(id)) {
      case null { return (false, "Reached all tiers.") };
      case (?tier) { (tier.status, tier.id) };
    };
    if (tierStatus == "complete") {
      return (false, "Tier already completed");
    };
    if (validateFunc.get(tierID)()) {
      switch (accounts.get(id)) {
        case null { return (false, "Player not found.") };
        case (?account) {
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
            alias = account.alias;
            tiers = updTiers;
            tokens = account.tokens;
          };
          accounts.put(id, updAcc);
          _accounts := Iter.toArray(accounts.entries());
          return (true, "Tier completed, token minted");
        };
      };
    } else { return (false, "Tier not completed yet.") };
  };

  public shared ({ caller }) func enrollPlayer(signupCode : ?Text, alias : Text) : async (Bool, Text) {
    switch (accounts.get(caller)) {
      case null {
        let code : Text = switch (signupCode) {
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
                alias = alias;
                tokens = [];
                netWorth = 0.0;
                multiplier = 0.0; //not implemented
              },
            );
            _accounts := Iter.toArray(accounts.entries());
            let textNotfound = "Referral code not provided or code not found";
            return (true, "Account enrrolled" # ", " # textNotfound);
          };
          case (?id) {
            let (minted, text) = await claimReferralToken(code, signupToken);
            if (minted) {
              accounts.put(
                caller,
                {
                  playerID = caller;
                  refByUUID = if (minted) { code } else { "" };
                  uuid = await generateUUID64();
                  alias = alias;
                  tiers = await getAlltiers();
                  tokens = [];
                  netWorth = 0.0;
                  multiplier = 0.0; //not implemented
                },
              );
              _accounts := Iter.toArray(accounts.entries());
              return (true, "Account enrrolled" # ", " # text);
            };
            return (false, text);
          };
        };
      };
      case (?_) {
        return (false, "Error. Account exists.");
      };
    };
  };

  public func enrollByPrincipal(signupCode : ?Text, principal : Principal, alias : Text) : async (Bool, Text) {
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
                alias = alias;
                tiers = await getAlltiers();
                tokens = [];
                netWorth = 0.0;
                multiplier = 0.0; //not implemented
              },
            );
            _accounts := Iter.toArray(accounts.entries());
            let textNotfound = "Referral code not provided or code not found";
            return (true, "Account enrrolled" # ", " # textNotfound);
          };
          case (?id) {
            let (minted, text) = await claimReferralToken(code, signupToken);
            if (minted) {
              accounts.put(
                principal,
                {
                  playerID = principal;
                  refByUUID = if (minted) { code } else { "" };
                  uuid = await generateUUID64();
                  alias = alias;
                  tiers = await getAlltiers();
                  tokens = [];
                  netWorth = 0.0;
                  multiplier = 0.0; //not implemented
                },
              );
              _accounts := Iter.toArray(accounts.entries());
              return (true, "Account enrrolled" # ", " # text);
            };
            return (false, text);
          };
        };
      };
      case (?_) {
        return (false, "Error. Account exists.");
      };
    };
  };

  private func signupLinkShare(id : Principal) : async Text {
    let route = "https://cosmicrafts.com/signup_prom/";
    let err = "https://cosmicrafts.com/signup_prom/account_not_found";
    switch (accounts.get(id)) {
      case (?refAccount) {
        let uuid : Text = refAccount.uuid;
        route # uuid;
      };
      case null err;
    };
  };

  private func principalByUUID(uuid : UUID) : async ?Principal {
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

  let tokenX : ICRC1Interface = actor ("br5f7-7uaaa-aaaaa-qaaca-cai") : ICRC1Interface;
  type ICRC1Interface = actor {
    icrc1_name : shared () -> async Text;
    icrc1_symbol : shared () -> async Text;
    icrc1_decimals : shared () -> async Nat8;
    icrc1_fee : shared () -> async TypesICRC1.Balance;
    icrc1_metadata : shared () -> async [TypesICRC1.MetaDatum];
    icrc1_total_supply : shared () -> async TypesICRC1.Balance;
    icrc1_minting_account : shared () -> async ?TypesICRC1.Account;
    icrc1_balance_of : shared (args : TypesICRC1.Account) -> async TypesICRC1.Balance;
    icrc1_supported_standards : shared () -> async [TypesICRC1.SupportedStandard];
    icrc1_transfer : shared (args : TypesICRC1.TransferArgs) -> async TypesICRC1.TransferResult;
    icrc1_pay_for_transaction : shared (args : TypesICRC1.TransferArgs, from : Principal) -> async TypesICRC1.TransferResult;
    mint : shared (args : TypesICRC1.Mint) -> async TypesICRC1.TransferResult;
    burn : shared (args : TypesICRC1.BurnArgs) -> async TypesICRC1.TransferResult;
    get_transactions : shared (req : TypesICRC1.GetTransactionsRequest) -> async TypesICRC1.GetTransactionsResponse;
    get_transaction : shared (i : TypesICRC1.TxIndex) -> async ?TypesICRC1.Transaction;
    deposit_cycles : shared () -> async ();
  };
  private func mintTokensStandalone(principalId : Principal, amount : Nat) : async (Bool, Text) {
    // Prepare mint arguments
    let _tokenXArgs : TypesICRC1.Mint = {
      to = { owner = principalId; subaccount = null };
      amount = amount;
      memo = null;
      created_at_time = ?Nat64.fromNat(Int.abs(Time.now()));
    };

    // Mint tokenX
    let tokenXMinted = await tokenX.mint(_tokenXArgs);

    // Handle tokenX minting result
    let tokenXResult = switch (tokenXMinted) {
      case (#Ok(_tid)) "{\"token\":\"Token X\", \"transaction_id\": " # Nat.toText(_tid) # ", \"amount\": " # Nat.toText(amount) # "}";
      case (#Err(_e)) Utils.handleMintError("Token X", _e);
    };

    // Check the result and return
    let success = switch (tokenXMinted) {
      case (#Ok(_tid)) true;
      case (#Err(_e)) false;
    };

    return (success, tokenXResult);
  };
};
