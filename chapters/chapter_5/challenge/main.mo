import Result "mo:base/Result";
import HashMap "mo:base/HashMap";
import TrieMap "mo:base/TrieMap";
import Principal "mo:base/Principal";
import Text "mo:base/Text";
import Buffer "mo:base/Buffer";
import Nat64 "mo:base/Nat64";
import Iter "mo:base/Iter";
import Blob "mo:base/Blob";
import Debug "mo:base/Debug";
import Option "mo:base/Option";
import Time "mo:base/Time";
import Array "mo:base/Array";
import Types "types";
actor {
    // For this level we need to make use of the code implemented in the previous projects.
    // The voting system will make use of previous data structures and functions.

    /////////////////
    //   TYPES    //
    ///////////////
    type Member = Types.Member;
    type Result<Ok, Err> = Types.Result<Ok, Err>;
    type HashMap<K, V> = Types.HashMap<K, V>;
    type Proposal = Types.Proposal;
    type ProposalContent = Types.ProposalContent;
    type ProposalId = Types.ProposalId;
    type Vote = Types.Vote;
    type DAOStats = Types.DAOStats;
    type HttpRequest = Types.HttpRequest;
    type HttpResponse = Types.HttpResponse;

    /////////////////
    // PROJECT #1 //
    ///////////////
    let goals = Buffer.Buffer<Text>(0);
    let name = "Motoko Bootcamp";
    var manifesto = "Empower the next generation of builders and make the DAO-revolution a reality";

    public shared query func getName() : async Text {
        return name;
    };

    public shared query func getManifesto() : async Text {
        return manifesto;
    };

    public func setManifesto(newManifesto : Text) : async () {
        manifesto := newManifesto;
        return;
    };

    public func addGoal(newGoal : Text) : async () {
        goals.add(newGoal);
        return;
    };

    public shared query func getGoals() : async [Text] {
        Buffer.toArray(goals);
    };

    /////////////////
    // PROJECT #2 //
    ///////////////
    let members = HashMap.HashMap<Principal, Member>(0, Principal.equal, Principal.hash);

    public shared ({ caller }) func addMember(member : Member) : async Result<(), Text> {
        switch (members.get(caller)) {
            case (null) {
                members.put(caller, member);
                return #ok();
            };
            case (?member) {
                return #err("Member already exists");
            };
        };
    };

    public shared ({ caller }) func updateMember(member : Member) : async Result<(), Text> {
        switch (members.get(caller)) {
            case (null) {
                return #err("Member does not exist");
            };
            case (?member) {
                members.put(caller, member);
                return #ok();
            };
        };
    };

    public shared ({ caller }) func removeMember() : async Result<(), Text> {
        switch (members.get(caller)) {
            case (null) {
                return #err("Member does not exist");
            };
            case (?member) {
                members.delete(caller);
                return #ok();
            };
        };
    };

    public query func getMember(p : Principal) : async Result<Member, Text> {
        switch (members.get(p)) {
            case (null) {
                return #err("Member does not exist");
            };
            case (?member) {
                return #ok(member);
            };
        };
    };

    public query func getAllMembers() : async [Member] {
        return Iter.toArray(members.vals());
    };

    public query func numberOfMembers() : async Nat {
        return members.size();
    };

    /////////////////
    // PROJECT #3 //
    ///////////////
    let ledger = HashMap.HashMap<Principal, Nat>(0, Principal.equal, Principal.hash);

    public query func tokenName() : async Text {
        return "Motoko Bootcamp Token";
    };

    public query func tokenSymbol() : async Text {
        return "MBT";
    };

    public func mint(owner : Principal, amount : Nat) : async Result<(), Text> {
        let balance = Option.get(ledger.get(owner), 0);
        ledger.put(owner, balance + amount);
        return #ok();
    };

    public func burn(owner : Principal, amount : Nat) : async Result<(), Text> {
        let balance = Option.get(ledger.get(owner), 0);
        if (balance < amount) {
            return #err("Insufficient balance to burn");
        };
        ledger.put(owner, balance - amount);
        return #ok();
    };

    func _burn(owner : Principal, amount : Nat) : () {
        let balance = Option.get(ledger.get(owner), 0);
        ledger.put(owner, balance - amount);
        return;
    };

    public shared ({ caller }) func transfer(from : Principal, to : Principal, amount : Nat) : async Result<(), Text> {
        let balanceFrom = Option.get(ledger.get(from), 0);
        let balanceTo = Option.get(ledger.get(to), 0);
        if (balanceFrom < amount) {
            return #err("Insufficient balance to transfer");
        };
        ledger.put(from, balanceFrom - amount);
        ledger.put(to, balanceTo + amount);
        return #ok();
    };

    public query func balanceOf(owner : Principal) : async Nat {
        return (Option.get(ledger.get(owner), 0));
    };

    public query func totalSupply() : async Nat {
        var total = 0;
        for (balance in ledger.vals()) {
            total += balance;
        };
        return total;
    };
    /////////////////
    // PROJECT #4 //
    ///////////////
    var nextProposalId : Nat64 = 0;
    let proposals = HashMap.HashMap<ProposalId, Proposal>(0, Nat64.equal, Nat64.toNat32);

    public shared ({ caller }) func createProposal(content : ProposalContent) : async Result<ProposalId, Text> {
        switch (members.get(caller)) {
            case (null) {
                return #err("The caller is not a member - cannot create a proposal");
            };
            case (?member) {
                let balance = Option.get(ledger.get(caller), 0);
                if (balance < 1) {
                    return #err("The caller does not have enough tokens to create a proposal");
                };
                // Create the proposal and burn the tokens
                let proposal : Proposal = {
                    id = nextProposalId;
                    content;
                    creator = caller;
                    created = Time.now();
                    executed = null;
                    votes = [];
                    voteScore = 0;
                    status = #Open;
                };
                proposals.put(nextProposalId, proposal);
                nextProposalId += 1;
                _burn(caller, 1);
                return #ok(nextProposalId - 1);
            };
        };
    };

    public query func getProposal(proposalId : ProposalId) : async ?Proposal {
        return proposals.get(proposalId);
    };

    public shared ({ caller }) func voteProposal(proposalId : ProposalId, vote : Vote) : async Result<(), Text> {
        // Check if the caller is a member of the DAO
        switch (members.get(caller)) {
            case (null) {
                return #err("The caller is not a member - canno vote one proposal");
            };
            case (?member) {
                // Check if the proposal exists
                switch (proposals.get(proposalId)) {
                    case (null) {
                        return #err("The proposal does not exist");
                    };
                    case (?proposal) {
                        // Check if the proposal is open for voting
                        if (proposal.status != #Open) {
                            return #err("The proposal is not open for voting");
                        };
                        // Check if the caller has already voted
                        if (_hasVoted(proposal, caller)) {
                            return #err("The caller has already voted on this proposal");
                        };
                        let balance = Option.get(ledger.get(caller), 0);
                        let multiplierVote = switch (vote.yesOrNo) {
                            case (true) { 1 };
                            case (false) { -1 };
                        };
                        let newVoteScore = proposal.voteScore + balance * multiplierVote;
                        var newExecuted : ?Time.Time = null;
                        let newVotes = Buffer.fromArray<Vote>(proposal.votes);
                        let newStatus = if (newVoteScore >= 100) {
                            #Accepted;
                        } else if (newVoteScore <= -100) {
                            #Rejected;
                        } else {
                            #Open;
                        };
                        switch (newStatus) {
                            case (#Accepted) {
                                _executeProposal(proposal.content);
                                newExecuted := ?Time.now();
                            };
                            case (_) {};
                        };
                        let newProposal : Proposal = {
                            id = proposal.id;
                            content = proposal.content;
                            creator = proposal.creator;
                            created = proposal.created;
                            executed = newExecuted;
                            votes = Buffer.toArray(newVotes);
                            voteScore = newVoteScore;
                            status = newStatus;
                        };
                        proposals.put(proposal.id, newProposal);
                        return #ok();
                    };
                };
            };
        };
    };

    func _hasVoted(proposal : Proposal, member : Principal) : Bool {
        return Array.find<Vote>(
            proposal.votes,
            func(vote : Vote) {
                return vote.member == member;
            },
        ) != null;
    };

    func _executeProposal(content : ProposalContent) : () {
        switch (content) {
            case (#ChangeManifesto(newManifesto)) {
                manifesto := newManifesto;
            };
            case (#AddGoal(newGoal)) {
                goals.add(newGoal);
            };
        };
        return;
    };

    public query func getAllProposals() : async [Proposal] {
        return Iter.toArray(proposals.vals());
    };

    /////////////////
    // PROJECT #5 //
    ///////////////
    let logo : Text = "<?xml version='1.0' standalone='no'?>
<!DOCTYPE svg PUBLIC '-//W3C//DTD SVG 20010904//EN'
 'http://www.w3.org/TR/2001/REC-SVG-20010904/DTD/svg10.dtd'>
<svg version='1.0' xmlns='http://www.w3.org/2000/svg'
 width='512.000000pt' height='512.000000pt' viewBox='0 0 512.000000 512.000000'
 preserveAspectRatio='xMidYMid meet'>

<g transform='translate(0.000000,512.000000) scale(0.100000,-0.100000)'
fill='#000000' stroke='none'>
<path d='M2875 4779 c-293 -30 -602 -158 -827 -342 l-58 -47 -399 0 c-365 0
-400 -1 -419 -17 -11 -10 -120 -175 -243 -368 -122 -192 -229 -358 -237 -367
-14 -16 -38 -18 -233 -18 -214 0 -218 0 -233 -22 -21 -30 -20 -44 4 -68 19
-19 33 -20 262 -20 204 0 244 2 259 15 9 9 121 180 250 380 l233 365 313 0
c249 0 313 -3 307 -12 -4 -7 -26 -35 -49 -63 -50 -59 -110 -148 -155 -227
l-32 -58 -108 0 c-99 0 -111 -2 -134 -22 -14 -13 -121 -158 -236 -323 -143
-204 -210 -306 -207 -320 4 -22 417 -614 443 -636 11 -9 49 -15 98 -18 l81 -3
40 -86 c22 -48 61 -122 88 -165 26 -43 47 -80 47 -83 0 -2 -116 -4 -258 -4
-255 0 -259 0 -279 23 -11 12 -127 175 -257 362 -131 187 -247 348 -258 358
-18 15 -44 17 -220 17 -198 0 -199 0 -223 -25 -14 -13 -25 -29 -25 -35 0 -6
11 -22 25 -35 24 -25 26 -25 204 -25 l180 0 256 -365 c142 -201 265 -370 275
-375 11 -6 152 -10 345 -10 l326 0 98 -97 c53 -54 141 -129 194 -167 l97 -68
-2 -510 -3 -511 -45 -11 c-101 -26 -165 -112 -164 -221 0 -91 40 -157 124
-201 33 -18 77 -19 879 -22 934 -3 904 -5 971 61 45 44 70 101 70 165 0 115
-72 201 -192 226 l-28 6 0 210 0 210 243 0 c257 0 301 6 355 46 65 50 66 56
72 404 7 420 -3 402 275 510 88 34 166 68 174 75 7 7 13 24 13 37 0 12 -72
156 -160 318 l-161 295 9 50 c17 106 22 263 11 375 -56 565 -388 1047 -894
1302 -269 134 -575 189 -882 157z m405 -120 c204 -30 456 -134 634 -261 297
-211 521 -553 601 -918 23 -106 31 -349 15 -495 l-12 -120 146 -268 c80 -148
146 -272 146 -277 0 -5 -60 -32 -132 -60 -175 -68 -211 -95 -266 -195 l-27
-50 -3 -300 c-1 -165 -5 -310 -7 -322 -3 -12 -15 -32 -27 -43 -21 -19 -34 -20
-308 -20 -242 0 -289 -2 -308 -16 -22 -15 -22 -18 -22 -270 l0 -254 -690 0
-690 0 0 533 c0 412 -3 537 -13 549 -6 8 -47 38 -89 66 -67 45 -107 77 -227
179 l-26 22 226 1 226 0 194 -291 c106 -160 202 -297 213 -305 15 -10 58 -14
171 -14 l150 -1 30 -52 c44 -76 107 -112 194 -112 103 1 168 43 210 135 40 87
22 180 -49 250 -72 73 -164 89 -255 46 -54 -25 -94 -64 -114 -115 l-13 -31
-133 0 -132 0 -145 218 c-79 119 -153 229 -162 245 l-18 27 244 0 244 0 18
-40 c33 -75 120 -130 206 -130 132 0 240 118 227 248 -24 226 -316 286 -428
87 l-31 -55 -584 0 -583 0 -60 88 c-32 48 -77 125 -100 170 l-41 82 960 0 960
0 14 -33 c34 -83 144 -145 232 -133 121 16 204 110 204 228 -1 126 -105 228
-234 228 -77 0 -162 -54 -196 -125 l-22 -45 -534 0 -534 0 160 240 160 240
168 0 167 0 27 -50 c36 -65 100 -103 183 -108 128 -8 235 92 235 220 0 67 -19
121 -58 161 -113 119 -314 84 -377 -65 l-15 -38 -188 0 -188 0 -26 -27 c-14
-16 -107 -151 -207 -300 l-181 -273 -379 0 -378 0 -21 78 c-30 110 -41 183
-49 300 l-6 102 190 0 191 0 6 -26 c4 -14 25 -45 47 -69 141 -153 389 -55 389
154 0 156 -143 264 -292 219 -51 -15 -124 -79 -140 -123 l-12 -35 -184 0 -185
0 6 68 c8 85 45 233 84 332 l29 75 250 3 251 2 13 -31 c21 -51 60 -90 115
-115 91 -43 183 -27 255 46 73 73 89 164 46 257 -44 95 -144 145 -245 124 -74
-15 -126 -51 -159 -111 l-27 -50 -218 0 -218 0 15 28 c25 48 127 185 194 260
l64 72 327 0 326 0 180 -80 180 -80 -3 -41 c-4 -66 23 -144 67 -188 144 -144
391 -42 391 160 0 64 -20 110 -68 158 -78 78 -170 92 -268 41 l-57 -31 -196
88 -196 88 -264 5 -264 5 55 37 c190 126 443 215 678 238 94 9 270 4 370 -11z
m53 -529 c28 -22 49 -80 40 -108 -20 -64 -56 -92 -119 -92 -54 0 -104 53 -104
110 0 34 6 48 34 76 29 29 41 34 79 34 29 0 53 -7 70 -20z m-840 -210 c45 -50
32 -136 -24 -166 -50 -25 -97 -18 -136 21 -33 33 -35 39 -30 81 8 72 51 107
122 99 29 -3 48 -12 68 -35z m-933 -133 c0 -2 -11 -35 -24 -73 -35 -99 -63
-215 -77 -316 l-12 -88 -174 0 -174 0 51 73 c28 39 104 147 168 239 l117 167
63 1 c34 0 62 -1 62 -3z m1908 -433 c27 -19 52 -65 52 -97 0 -37 -41 -91 -80
-103 -121 -40 -203 117 -102 196 30 24 98 26 130 4z m-1239 -20 c80 -67 31
-194 -76 -194 -36 0 -50 6 -73 29 -33 33 -45 69 -36 107 4 15 20 39 36 55 39
39 105 41 149 3z m-789 -166 c0 -100 34 -327 66 -435 6 -21 3 -23 -32 -23 -39
1 -39 1 -156 168 -64 92 -139 200 -167 240 l-52 72 171 0 c170 0 170 0 170
-22z m2459 -437 c26 -25 31 -36 31 -76 0 -40 -5 -52 -34 -81 -40 -40 -83 -45
-132 -15 -95 58 -52 201 61 201 34 0 50 -6 74 -29z m-543 -455 c85 -85 5 -217
-112 -184 -44 12 -74 55 -74 108 0 34 6 48 34 76 28 28 42 34 76 34 34 0 48
-6 76 -34z m61 -576 c8 0 28 -13 44 -29 24 -24 29 -38 29 -76 0 -40 -5 -52
-34 -81 -28 -28 -42 -34 -76 -34 -34 0 -48 6 -76 34 -38 38 -45 85 -20 134 18
35 75 65 104 56 8 -2 20 -4 29 -4z m453 -1045 c32 -17 60 -61 60 -95 0 -29
-26 -76 -52 -94 -20 -14 -117 -16 -855 -16 -624 0 -839 3 -858 12 -33 15 -55
56 -55 104 0 28 8 44 34 70 l34 34 831 0 c724 0 836 -2 861 -15z'/>
<path d='M3172 4488 c-14 -14 -16 -57 -3 -76 9 -15 70 -31 118 -32 24 0 37 7
50 26 15 23 15 29 2 52 -8 14 -25 29 -39 33 -41 12 -115 10 -128 -3z'/>
<path d='M3462 4428 c-27 -13 -38 -49 -23 -75 15 -29 115 -65 145 -53 54 20
44 83 -17 109 -64 28 -80 30 -105 19z'/>
<path d='M3657 4322 c-10 -10 -17 -31 -17 -45 0 -22 11 -34 54 -62 339 -216
541 -560 574 -975 6 -88 9 -95 33 -109 24 -13 29 -13 54 3 l28 18 -6 107 c-16
326 -153 629 -391 866 -102 102 -257 215 -293 215 -11 0 -27 -8 -36 -18z'/>
<path d='M550 3303 c-41 -15 -51 -62 -20 -93 18 -18 33 -20 143 -20 139 0 167
10 167 60 0 50 -28 60 -160 59 -63 0 -122 -3 -130 -6z'/>
<path d='M1068 2043 c-9 -10 -20 -47 -27 -82 -6 -35 -12 -64 -13 -65 -16 -9
-115 -46 -122 -46 -5 0 -30 16 -56 35 -66 49 -85 46 -151 -22 -95 -99 -139
-152 -139 -166 0 -8 17 -41 39 -74 l38 -59 -25 -65 c-15 -35 -27 -65 -27 -67
-1 -1 -28 -7 -61 -13 -93 -17 -94 -19 -94 -185 0 -113 3 -146 15 -157 13 -13
94 -37 128 -37 7 0 23 -31 37 -68 l26 -68 -38 -58 c-21 -33 -38 -67 -38 -77 0
-10 43 -64 96 -119 112 -116 125 -120 197 -65 25 19 50 35 55 35 24 0 121 -53
126 -68 3 -9 8 -38 11 -64 12 -82 14 -83 174 -83 113 0 143 3 156 15 8 9 21
46 28 84 l12 67 63 25 62 26 49 -36 c75 -55 87 -51 196 62 67 69 94 104 95
122 0 14 -16 49 -36 78 l-36 54 25 66 24 67 64 13 c95 18 99 26 99 188 0 156
-6 168 -87 180 -26 3 -54 10 -61 14 -12 6 -62 113 -62 133 0 4 16 31 35 60 19
28 35 63 35 76 0 32 -184 221 -215 221 -12 0 -46 -17 -76 -38 l-54 -39 -61 28
-62 28 -12 68 c-6 37 -16 73 -22 80 -8 9 -50 13 -153 13 -123 0 -144 -2 -157
-17z m227 -165 c12 -67 22 -78 90 -103 28 -9 69 -28 92 -41 56 -32 77 -30 132
11 l48 34 41 -42 c23 -24 42 -46 42 -51 0 -4 -13 -27 -30 -51 -36 -53 -37 -81
-6 -143 13 -26 30 -67 37 -92 18 -63 32 -76 100 -89 l60 -12 -3 -67 -3 -66
-56 -11 c-30 -6 -60 -15 -66 -18 -6 -4 -21 -38 -34 -75 -12 -37 -30 -84 -40
-104 -26 -51 -24 -74 13 -130 l31 -50 -44 -44 -45 -44 -39 29 c-63 45 -75 46
-151 7 -38 -19 -83 -38 -99 -42 -45 -10 -62 -30 -70 -82 -11 -83 -10 -82 -75
-82 l-58 0 -11 61 c-12 73 -27 92 -84 109 -23 7 -68 25 -100 42 -66 33 -82 31
-141 -15 l-39 -31 -43 48 -44 47 35 52 c19 28 35 58 35 65 0 7 -11 34 -24 60
-13 26 -31 72 -41 102 -24 75 -38 88 -101 96 l-54 6 0 69 c0 67 1 69 28 74 78
16 99 26 110 50 7 14 12 31 12 38 0 6 17 46 37 89 40 86 40 89 -16 173 l-20
30 44 46 44 46 47 -33 c55 -40 76 -42 124 -13 26 16 154 69 167 69 3 0 22 67
28 103 l6 38 62 -3 61 -3 11 -57z'/>
<path d='M1108 1565 c-62 -23 -122 -70 -161 -127 -123 -180 -62 -425 130 -520
123 -60 265 -40 367 52 35 33 63 70 82 112 25 55 29 75 29 153 0 82 -3 97 -33
157 -61 124 -166 189 -305 187 -40 0 -89 -7 -109 -14z m230 -138 c89 -59 125
-184 82 -285 -87 -202 -359 -176 -410 38 -25 105 25 209 125 264 48 26 152 18
203 -17z'/>
</g>
</svg>
";

    func _getWebpage() : Text {
        var webpage = "<style>" #
        "body { text-align: center; font-family: Arial, sans-serif; background-color: #f0f8ff; color: #333; }" #
        "h1 { font-size: 3em; margin-bottom: 10px; }" #
        "hr { margin-top: 20px; margin-bottom: 20px; }" #
        "em { font-style: italic; display: block; margin-bottom: 20px; }" #
        "ul { list-style-type: none; padding: 0; }" #
        "li { margin: 10px 0; }" #
        "li:before { content: '👉 '; }" #
        "svg { max-width: 150px; height: auto; display: block; margin: 20px auto; }" #
        "h2 { text-decoration: underline; }" #
        "</style>";

        webpage := webpage # "<div><h1>" # name # "</h1></div>";
        webpage := webpage # "<em>" # manifesto # "</em>";
        webpage := webpage # "<div>" # logo # "</div>";
        webpage := webpage # "<hr>";
        webpage := webpage # "<h2>Our goals:</h2>";
        webpage := webpage # "<ul>";
        for (goal in goals.vals()) {
            webpage := webpage # "<li>" # goal # "</li>";
        };
        webpage := webpage # "</ul>";
        return webpage;
    };

    public query func getStats() : async DAOStats {
        let arrayMembers = Iter.toArray(members.vals());
        let namesOfMembers = Array.map<Member, Text>(arrayMembers, func item = item.name);
        return ({
            name;
            manifesto;
            goals = Buffer.toArray(goals);
            members = namesOfMembers;
            logo;
            numberOfMembers = namesOfMembers.size();
        });
    };

    public func http_request(request : HttpRequest) : async HttpResponse {
        return ({
            headers = [("Content-Type", "text/html; charset=UTF-8")];
            status_code = 200 : Nat16;
            body = Text.encodeUtf8(_getWebpage());
            streaming_strategy = null;
        });
    };

};