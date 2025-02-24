import Result "mo:base/Result";
import Principal "mo:base/Principal";
import HashMap "mo:base/HashMap";
import Array "mo:base/Array";
import Iter "mo:base/Iter";
import Types "types";
actor {

    type Result<Ok, Err> = Types.Result<Ok, Err>;
    type HashMap<K, V> = Types.HashMap<K, V>;

    let ledger: HashMap<Principal, Nat> = HashMap.HashMap<Principal, Nat>(1, Principal.equal, Principal.hash);

    public query func tokenName() : async Text {
        return "LFinance";
    };

    public query func tokenSymbol() : async Text {
        return "LFI";
    };

    public func mint(owner : Principal, amount : Nat) : async Result<(), Text> {
        switch (ledger.get(owner)) {
            case(?tokenAmount) {
                ledger.put(owner, tokenAmount + amount);
                return #ok();
            };
            case null {
                ledger.put(owner, amount);
                return #ok();
            }
        };
    };

    public func burn(owner : Principal, amount : Nat) : async Result<(), Text> {
        switch (ledger.get(owner)) {
            case(?tokenAmount) {
                if (tokenAmount < amount) {
                    return #err("Balance are not enough!");
                };
                ledger.put(owner, tokenAmount - amount);
                return #ok();
            };
            case null {
                return #err("User with this principal do not exist!");
            }
        };
    };

    public shared ({ caller }) func transfer(from : Principal, to : Principal, amount : Nat) : async Result<(), Text> {
        if (caller != from) {
            return #err("You are not owner of this tokens!")
        };

        switch (await burn(from, amount)) {
            case (#err(e)) return #err(e);
            case (#ok(())) {};
        };

        switch (await mint(to, amount)) {
            case (#err(e)) return #err(e);
            case (#ok(())) {};
        };

        return #ok();
    };

    public query func balanceOf(account : Principal) : async Nat {
        switch(ledger.get(account)) {
            case null return 0;
            case (?tokenAmount) return tokenAmount;
        };
    };

    public query func totalSupply() : async Nat {
        let values = Iter.toArray(ledger.vals());
        return Array.foldLeft<Nat, Nat>(values, 0, func(acc, value) { acc + value });
    };

};