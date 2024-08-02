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
import Text "mo:base/Text";
import Int64 "mo:base/Int64";
import Timer "mo:base/Timer";

import TypesICRC1 "../icrc1/Types";
import Utils "Utils";

actor {

  type UUID = Text;
  type TierID = Nat;

  type RefAccount = {
    playerID : Principal;
    refByUUID : UUID;
    uuid : UUID;
    alias : Text;
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
  type RefAccView = {
    playerID : Principal;
    playerName : Text;
    currentTier : Tier;
    multiplier : Float;
    netWorth : Nat;
    topPlayers : [TopView];
    topPosition : Nat;
    topTokenAmount : (Nat, Text);
    signupTokenSum : Nat;
    tierTokenSum : Nat;
    singupLink : Text;
  };
  type TopView = {
    playerName : Text;
    multiplier : Float;
    netWorth : Nat;
  };

  type F = () -> Bool;
  type BufferF = Buffer.Buffer<F>;
  private var validateFunc : BufferF = Buffer.Buffer<F>(0);

  private stable var _tiers : [Tier] = [];
  private stable var _refTokens : [(Principal, [Token])] = [];
  private stable var _accounts : [(Principal, RefAccount)] = [];

  type Buffer = Buffer.Buffer<Tier>;
  private var tiers : Buffer = Buffer.fromArray<Tier>(_tiers);

  type HashMapRef = HashMap.HashMap<Principal, [Token]>;
  type HashMapAcc = HashMap.HashMap<Principal, RefAccount>;

  private var refTokens : HashMapRef = HashMap.fromIter(
    Iter.fromArray(_refTokens),
    0,
    Principal.equal,
    Principal.hash,
  );
  private var accounts : HashMapAcc = HashMap.fromIter(
    Iter.fromArray(_accounts),
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
    amount = 10;
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
    token = { title = "Tier 3 Tweeter token"; amount = 10 };
  };
  tiers.add(tweeterTier);

  let tiersCompleted : Tier = {
    id = 3;
    title = "All tiers defeated";
    desc = "You reached a Master referral record";
    status = "Waiting for more tiers";
    token = { title = "No token"; amount = 0 };
  };
  tiers.add(tiersCompleted);

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

  public shared ({ caller }) func enroll(signupCode : ?Text, alias : Text) : async (Bool, Text) {
    switch (accounts.get(caller)) {
      case null {
        let code : Text = switch (signupCode) {
          case (null) { "" };
          case (?value) { value };
        };

        switch (id_from_uuid(code)) {
          case (null) {

            accounts.put(
              caller,
              {
                playerID = caller;
                refByUUID = "";
                uuid = await uuid_gen();
                tiers = await tier_all();
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

          case (?_) {

            let (minted, text) = await claim_referral(
              code,
              signupToken,
            );

            if (minted) {

              accounts.put(
                caller,
                {
                  playerID = caller;
                  refByUUID = if (minted) { code } else { "" };
                  uuid = await uuid_gen();
                  alias = alias;
                  tiers = await tier_all();
                  tokens = [];
                  netWorth = 0.0;
                  multiplier = 0.0;
                },
              );

              _accounts := Iter.toArray(accounts.entries());

              return (true, "Account enrrolled" # ", " # text);
            };
            return (false, text);
          };
        };
      };
      case (?_) { return (false, "Error. Account exists.") };
    };
  };
  public func enroll_by(uuid : ?Text, principal : Principal, alias : Text) : async (Bool, Text) {
    switch (accounts.get(principal)) {
      case null {
        let code : Text = switch (uuid) {
          case (null) { "" };
          case (?value) { value };
        };

        var id = id_from_uuid(code);

        switch (id) {
          case (null) {

            accounts.put(
              principal,
              {
                playerID = principal;
                refByUUID = "";
                uuid = await uuid_gen();
                alias = alias;
                tiers = await tier_all();
                tokens = [];
                netWorth = 0.0;
                multiplier = 0.0; //not implemented
              },
            );

            _accounts := Iter.toArray(accounts.entries());

            let nullUUID = "Signup code no provided or code not found";
            return (true, "Account enrrolled" # ", " # nullUUID);
          };

          case (?id) {

            let (minted, text) = await claim_referral(
              code,
              signupToken,
            );

            if (minted) {
              accounts.put(
                principal,
                {
                  playerID = principal;
                  refByUUID = if (minted) { code } else { "" };
                  uuid = await uuid_gen();
                  alias = alias;
                  tiers = await tier_all();
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
      case (?_) { (false, "Error. Account exists.") };
    };
  };
  public query func account_view(id : Principal) : async ?RefAccView {
    let account = switch (accounts.get(id)) {
      case null { return null };
      case (?acc) { acc };
    };
    let currentTier = switch (tier_p(id)) {
      case null { tiersCompleted };
      case (?tier) tier;
    };
    let (
      multiplier,
      networth,
      tierTokenSum,
      signupTokenSum,
    ) = tokenomics(account);

    let pageTop10 = 0;

    return ?({
      playerID = id;
      playerName = account.alias;
      currentTier = currentTier;
      multiplier = multiplier;
      netWorth = networth;
      tierTokenSum = tierTokenSum;
      signupTokenSum = signupTokenSum;
      topPlayers = top_view(pageTop10);
      topPosition = player_rank(id);
      topTokenAmount = top_prize(id);
      singupLink = signup_link(id);
    });
  };
  public query func account_by(id : Principal) : async ?RefAccount {
    return accounts.get(id);
  };
  public query ({ caller }) func account() : async ?RefAccount {
    return accounts.get(caller);
  };
  public query func account_all() : async [(Text, Principal)] {
    let account = Iter.toArray(accounts.vals());
    let buffer = Buffer.Buffer<(Text, Principal)>(account.size());
    for (acc in account.vals()) buffer.add(acc.alias, acc.playerID);
    return Buffer.toArray(buffer);
  };
  public func claim_top(id : Principal, day : Nat) : async (Bool, Text) {
    let account = switch (accounts.get(id)) {
      case null { return (false, "Account not found") };
      case (?account) { account };
    };

    if (day == 1) {
      let (tokenAmount, _) = top_prize(id);
      if (tokenAmount > 0) {

        let (multiplier, _, _, _) = tokenomics(account);
        let total = token_amount(multiplier, tokenAmount);
        let (minted, _) = await mint(id, total);

        if (minted) {
          let token : Token = {
            title = "Weekly Top Player Token";
            amount = total;
          };
          let updatedTokens = Array.append(
            account.tokens,
            [token],
          );
          let updatedAccount : RefAccount = {
            playerID = account.playerID;
            refByUUID = account.refByUUID;
            uuid = account.uuid;
            alias = account.alias;
            tiers = account.tiers;
            tokens = updatedTokens;
          };

          accounts.put(id, updatedAccount);
          _accounts := Iter.toArray(accounts.entries());

          return (true, "Weekly top player token claimed: ");
        } else {
          return (false, "Error minting weekly top player token");
        };
      } else {
        return (false, "Player not in top 10.");
      };
    } else {
      return (false, "Only on moday's may be claimed");
    };
  };
  public func claim_tier(id : Principal) : async (Bool, Text) {

    let (tierStatus, tierID) = switch (tier_p(id)) {
      case null { return (false, "Reached all tiers.") };
      case (?tier) { (tier.status, tier.id) };
    };

    if (tierStatus == "No more tiers") { return (false, "No more tiers") };
    if (tierStatus == "complete") { return (false, "Tier already completed") };

    if (validateFunc.get(tierID)()) {
      switch (accounts.get(id)) {
        case null { return (false, "Player not found.") };

        case (?account) {

          let tokenAmount = account.tiers[tierID].token.amount;
          let (multiplier, _, _, _) = tokenomics(account);
          let total = token_amount(multiplier, tokenAmount);
          let (minted, result) = await mint(id, total);

          if (minted) {
            let updTiers = Array.tabulate<Tier>(
              Array.size(account.tiers),
              func(i : Nat) : Tier {
                if (i == tierID) {
                  let updTier : Tier = {
                    id = account.tiers[i].id;
                    title = account.tiers[i].title;
                    desc = account.tiers[i].desc;
                    status = "Complete";
                    token = {
                      title = account.tiers[i].token.title;
                      amount = total;
                    };
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

            return (true, "Tier complete, token minted");
          } else {
            return (false, result);
          };
        };
      };
    } else { return (false, "Tier not completed yet.") };
  };
  public query func tier_all() : async [Tier] {
    return Buffer.toArray(tiers);
  };
  public func id_gen() : async Principal {
    let randomBytes = await Random.blob();
    let randomArray = Blob.toArray(randomBytes);
    let truncatedBytes = Array.tabulate<Nat8>(
      29,
      func(i : Nat) : Nat8 {
        if (i < Array.size(randomArray)) randomArray[i] else 0;
      },
    );
    return Principal.fromBlob(Blob.fromArray(truncatedBytes));
  };

  private func tier_p(playerId : Principal) : ?Tier {
    let player = accounts.get(playerId);
    switch (player) {
      case (null) { return null };

      case (?player) {
        for (tier in player.tiers.vals()) {
          if (tier.status == "Progress") {
            return ?tier;
          };
        };

        return null;
      };
    };
  };
  private func player_rank(id : Principal) : Nat {
    let playersArray = Buffer.fromArray<(Principal, RefAccount)>(Iter.toArray(accounts.entries()));
    var playersWithTokenSums : [(Principal, Nat)] = [];

    for (i in Iter.range(0, playersArray.size() - 1)) {
      let (principal, account) = playersArray.get(i);
      let (_, tokenSum, _, _) = tokenomics(account);
      playersWithTokenSums := Array.append(playersWithTokenSums, [(principal, tokenSum)]);
    };

    let sortedPlayers = Array.sort(
      playersWithTokenSums,
      func(a : (Principal, Nat), b : (Principal, Nat)) : {
        #less;
        #equal;
        #greater;
      } {
        if (a.1 > b.1) {
          #less;
        } else if (a.1 < b.1) {
          #greater;
        } else {
          #equal;
        };
      },
    );

    var position : Nat = 0;
    for ((principal, _) in sortedPlayers.vals()) {
      if (principal == id) {
        return position + 1;
      };
      position += 1;
    };

    0;
  };
  private func top_prize(id : Principal) : (Nat, Text) {
    let topPlayers = top_view(0);
    switch (accounts.get(id)) {
      case (null) { return (0, "Account not found") };

      case (?account) {
        for (player in topPlayers.vals()) {
          if (player.playerName == account.alias) {
            for (token in account.tokens.vals()) {
              if (token.title == "Weekly Top Player Token") {
                return (token.amount, "Tokens claimed");
              };
            };
            let prizeAmount = 10;
            let (multiplier, _, _, _) = tokenomics(account);
            let total = token_amount(multiplier, prizeAmount);
            return (total, "You are in, waiting for monday.");
          };
        };

        return (0, "Not clasified");
      };
    };
  };
  private func tokenomics(acc : RefAccount) : (Float, Nat, Nat, Nat) {
    var multiplier : Float = 0.0;
    let tierTokenSum : Nat = tier_token_sum(acc);
    let signupTokenSum : Nat = ref_token_sum(acc);
    let networth : Nat = tierTokenSum + signupTokenSum;

    if (networth <= 10) {
      multiplier := 1.3;
    } else if (networth <= 20) {
      multiplier := 2.2;
    } else {
      multiplier := 3.7;
    };

    (
      multiplier,
      networth,
      tierTokenSum,
      signupTokenSum,
    );
  };
  private func token_amount(multiplier : Float, nTokens : Nat) : Nat {
    let nat64 = Nat64.fromNat(nTokens);
    let int64 = Int64.fromNat64(nat64);
    let totalTokens = Float.fromInt64(int64);
    let total = Float.toInt64(multiplier * totalTokens);
    let nat = Int64.toNat64(total);
    return Nat64.toNat(nat);
  };
  private func ref_token_sum(account : RefAccount) : Nat {
    return Array.foldLeft<Token, Nat>(
      account.tokens,
      0,
      func(acc, token) {
        acc + token.amount;
      },
    );
  };
  private func tier_token_sum(account : RefAccount) : Nat {
    return Array.foldLeft<Tier, Nat>(
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
  };
  private func top_view(page : Nat) : [TopView] {
    var playersWithTokenSums : [(Principal, RefAccount, Nat)] = [];
    let playersArray = Buffer.fromArray<(Principal, RefAccount)>(
      Iter.toArray(accounts.entries())
    );

    for (i in Iter.range(0, playersArray.size() - 1)) {
      let (principal, account) = playersArray.get(i);
      let (multiplier, networth, _, _) = tokenomics(account);
      let tokenSum = networth;

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

    let sorted = Array.sort(
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
    let end = if (start + 10 > Array.size(sorted)) {
      Array.size(sorted);
    } else { start + 10 };

    let paginated = Iter.toArray(
      Array.slice(
        sorted,
        start,
        end,
      )
    );

    var viewArray : [TopView] = [];

    for ((_, refAccount, _) in paginated.vals()) {
      let (m, n, _, _) = tokenomics(refAccount);
      let rowView : TopView = {
        playerName = refAccount.alias;
        multiplier = m;
        netWorth = n;
      };
      viewArray := Array.append(viewArray, [rowView]);
    };

    viewArray;
  };
  private func claim_referral(code : UUID, token : Token) : async (Bool, Text) {
    let id = switch (id_from_uuid(code)) {
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

        if (Array.size(account.tokens) > 3) {
          return (false, "Reached max referral per player");
        };

        if (size > 0) {

          let (multiplier, _, _, _) = tokenomics(account);
          let total = token_amount(multiplier, signupToken.amount);
          let (minted, result) = await mint(id, total);

          if (minted) {

            let updAcc : RefAccount = {
              playerID = account.playerID;
              refByUUID = account.refByUUID;
              uuid = account.uuid;
              alias = account.alias;
              tiers = account.tiers;
              tokens = Array.append(
                Iter.toArray(refTokens.vals())[0],
                [{ title = token.title; amount = total }],
              );
            };

            refTokens.put(account.playerID, updAcc.tokens);
            _refTokens := Iter.toArray(refTokens.entries());
            accounts.put(account.playerID, updAcc);
            _accounts := Iter.toArray(accounts.entries());

            return (true, "Referral token added to account.");
          } else {
            return (false, "Error miniting tokens." # result);
          };
        } else {

          let (multiplier, _, _, _) = tokenomics(account);
          let total = token_amount(multiplier, signupToken.amount);
          let (minted, result) = await mint(id, total);

          if (minted) {
            let updAcc : RefAccount = {
              playerID = account.playerID;
              refByUUID = account.refByUUID;
              uuid = account.uuid;
              alias = account.alias;
              tiers = account.tiers;
              tokens = [{ title = token.title; amount = total }];
            };

            refTokens.put(account.playerID, updAcc.tokens);
            _refTokens := Iter.toArray(refTokens.entries());
            accounts.put(account.playerID, updAcc);
            _accounts := Iter.toArray(accounts.entries());

            return (true, "First referral token added to account. ");
          } else {
            return (false, "Error miniting tokens." # result);
          };
        };
        return (false, "Mint token error.");
      };
    };
  };
  private func signup_link(id : Principal) : Text {
    let route = "https://cosmicrafts.com/signup_prom/";
    let err = "Account not found";
    switch (accounts.get(id)) {
      case (?refAccount) {
        let uuid : Text = refAccount.uuid;
        route # uuid;
      };
      case null err;
    };
  };
  private func id_from_uuid(uuid : UUID) : ?Principal {
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
    switch (mappedIter.next()) {
      case (null) { null };
      case (?(principal, _)) { ?principal };
    };
  };
  private func uuid_gen() : async Text {
    var uuid : Nat = 0;
    let randomBytes = await Random.blob();
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

  //Matches

  type RPlayer = {
    id : Principal;
    name : Text;
    balance : Float;
  };

  type Status = {
    #waiting;
    #progress;
    #complete;
  };

  type RMatch = {
    id : Text;
    name : Text;
    player1 : Principal;
    player2 : ?Principal;
    fee : Float;
    entry : Float;
    price : Float;
    status : Status;
    winner : ?Principal;
    date : Time.Time;
  };

  stable var _matches : [RMatch] = [];
  var matchesBuffer : Buffer.Buffer<RMatch> = Buffer.Buffer<RMatch>(0);

  public func createMatch(player1 : RPlayer, entry : Float) : async RMatch {

    let matchId = await uuid_gen();
    let fee = entry * 2.0 * 0.1;
    let price = (entry * 2) - fee;

    let newMatch : RMatch = {
      id = matchId;
      name = player1.name;
      player1 = player1.id;
      player2 = null;
      fee = fee;
      entry = entry;
      price = price;
      status = #waiting;
      winner = null;
      date = Time.now();
    };

    matchesBuffer.add(newMatch);
    _matches := Array.append(_matches, [newMatch]);

    let duration : Timer.Duration = #seconds(3600);
    let _ = Timer.setTimer<system>(
      duration,
      func() : async () {

        matchesBuffer.filterEntries(
          func(_ : Nat, m : RMatch) : Bool {
            m.id != matchId;
          }
        );

        _matches := Array.foldLeft(
          _matches,
          [],
          func(acc : [RMatch], m : RMatch) : [RMatch] {
            if (m.id != matchId) {
              Array.append(acc, [m]);
            } else {
              acc;
            };
          },
        );
      },
    );
    return newMatch;
  };

  public func joinMatch(player2 : RPlayer, matchId : Text) : async ?RMatch {

    let maybeMatch = await findMatchById(matchId);

    switch maybeMatch {
      case null { return null };

      case (?match) {
        if (match.status == #waiting) {
          let updatedMatch = {
            match with
            player2 = ?player2.id;
            status = #progress;
          };
          updateMatchInBufferAndStable(updatedMatch);
          return ?updatedMatch;
        } else {
          return null;
        };
      };
    };
  };

  public func completeMatch(matchId : Text, winner : Principal) : async ?RMatch {

    let maybeMatch = await findMatchById(matchId);

    switch maybeMatch {
      case null { return null };

      case (?match) {
        if (match.status == #progress) {

          let updatedMatch = {
            match with
            winner = ?winner;
            status = #complete;
          };

          updateMatchInBufferAndStable(updatedMatch);
          // Grant the prize to the winner and commission to the host
          _matches := Array.append(_matches, [updatedMatch]);

          return ?updatedMatch;
        } else {
          return null;
        };
      };
    };
  };

  public func findMatchById(matchId : Text) : async ?RMatch {
    for (match in Iter.fromArray(Buffer.toArray(matchesBuffer))) {
      if (match.id == matchId) {
        return ?match;
      };
    };
    return null;
  };

  public func grantPrize(match : RMatch) : async ?Principal {
    return switch (match.winner) {
      case null { return null };
      case (winner) { winner };
    };
  };
  private func updateMatchInBufferAndStable(updatedMatch : RMatch) {
    for (i in Iter.range(0, matchesBuffer.size() - 1)) {
      if (matchesBuffer.get(i).id == updatedMatch.id) {
        matchesBuffer.put(i, updatedMatch);
      };
    };
    _matches := Array.foldLeft(
      _matches,
      [],
      func(acc : [RMatch], m : RMatch) : [RMatch] {
        if (m.id == updatedMatch.id) {
          Array.append(acc, [updatedMatch]);
        } else {
          Array.append(acc, [m]);
        };
      },
    );
  };

  let tokenX : ICRC1Interface = actor ("bkyz2-fmaaa-aaaaa-qaaaq-cai") : ICRC1Interface;
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
  private func mint(principalId : Principal, amount : Nat) : async (Bool, Text) {
    let _tokenXArgs : TypesICRC1.Mint = {
      to = { owner = principalId; subaccount = null };
      amount = amount;
      memo = null;
      created_at_time = ?Nat64.fromNat(Int.abs(Time.now()));
    };
    let tokenXMinted = await tokenX.mint(_tokenXArgs);
    let tokenXResult = switch (tokenXMinted) {
      case (#Ok(_tid)) "{\"token\":\"Token X\", \"transaction_id\": " # Nat.toText(_tid) # ", \"amount\": " # Nat.toText(amount) # "}";
      case (#Err(_e)) Utils.handleMintError("Token X", _e);
    };
    let success = switch (tokenXMinted) {
      case (#Ok(_tid)) true;
      case (#Err(_e)) false;
    };
    (success, tokenXResult);
  };
};
