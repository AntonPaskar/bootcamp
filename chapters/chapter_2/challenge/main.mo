import Result "mo:base/Result";
import HashMap "mo:base/HashMap";
import Principal "mo:base/Principal";
import Iter "mo:base/Iter";
import Types "types";
actor {

    type Member = Types.Member;
    type Result<Ok, Err> = Types.Result<Ok, Err>;
    type HashMap<K, V> = Types.HashMap<K, V>;

    let members: HashMap<Principal,Member> = HashMap.HashMap<Principal, Member>(1, Principal.equal, Principal.hash);

    public shared ({ caller }) func addMember(member : Member) : async Result<(), Text> {
        if (members.get(caller) != null) {
            return #err("The caller already exists!");
        };
        members.put(caller, member);
        return #ok();
    };

    public query func getMember(p : Principal) : async Result<Member, Text> {
        switch (members.get(p)) {
            case (?searcheMember) return #ok(searcheMember);
            case null return #err("Searched member does not exist!");
        };
    };

    public shared ({ caller }) func updateMember(member : Member) : async Result<(), Text> {
        if (members.get(caller) == null) {
            return #err("Caller member are not exist!");
        };
        members.put(caller, member);
        return #ok();
    };

    public query func getAllMembers() : async [Member] {
        return Iter.toArray(members.vals());
    };

    public query func numberOfMembers() : async Nat {
        return members.size();
    };

    public shared ({ caller }) func removeMember() : async Result<(), Text> {
        if (members.get(caller) == null) {
            return #err("Caller member are not exist!")
        };
        members.delete(caller);
        return #ok();
    };

};