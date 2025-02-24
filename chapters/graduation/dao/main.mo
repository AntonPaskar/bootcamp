import Result "mo:base/Result";
import Text "mo:base/Text";
import Principal "mo:base/Principal";
import Nat "mo:base/Nat";
import HashMap "mo:base/HashMap";
import Int "mo:base/Int";
import Time "mo:base/Time";
import Iter "mo:base/Iter";
import Array "mo:base/Array";
import Buffer "mo:base/Buffer";
import Types "types";
actor {
        type Result<A, B> = Result.Result<A, B>;
        type Member = Types.Member;
        type ProposalContent = Types.ProposalContent;
        type ProposalId = Types.ProposalId;
        type Proposal = Types.Proposal;
        type Vote = Types.Vote;
        type HttpRequest = Types.HttpRequest;
        type HttpResponse = Types.HttpResponse;

        // The principal of the Webpage canister associated with this DAO canister (needs to be updated with the ID of your Webpage canister)
        stable let canisterIdWebpage : Principal = Principal.fromText("wqbl4-ayaaa-aaaab-qadga-cai");
        stable var manifesto = "Let's graduate!";
        stable let name = "LFinance";
        let members = HashMap.HashMap<Principal, Member>(0, Principal.equal, Principal.hash);
        let goals : Buffer.Buffer<Text> = Buffer.Buffer<Text>(0);

        let initialMentor : Member = {
                name = "motoko_bootcamp";
                role = #Mentor;
        };
        let initialMentorPrincipal : Principal = Principal.fromText("nkqop-siaaa-aaaaj-qa3qq-cai");
        members.put(initialMentorPrincipal, initialMentor);

        var nextProposalId : Nat = 0;
        let proposals = HashMap.HashMap<ProposalId, Proposal>(0, Nat.equal, Int.hash);

        let tokenCanister = actor ("jaamb-mqaaa-aaaaj-qa3ka-cai") : actor {
                mint : (owner : Principal, amount : Nat) -> async Result<(), Text>;
                burn : (owner : Principal, amount : Nat) -> async Result<(), Text>;
                transfer : shared (from : Principal, to : Principal, amount : Nat) -> async Result<(), Text>;
                balanceOf : (owner : Principal) -> async Nat;
        };

        let webpageCanister = actor (Principal.toText(canisterIdWebpage)) : actor {
                setManifesto : shared (newManifesto : Text) -> async Result<(), Text>;
        };

        // Returns the name of the DAO
        public query func getName() : async Text {
                return name;
        };

        // Returns the manifesto of the DAO
        public query func getManifesto() : async Text {
                return manifesto;
        };

        // Returns the goals of the DAO
        public query func getGoals() : async [Text] {
                return Buffer.toArray(goals);
        };

        // Register a new member in the DAO with the given name and principal of the caller
        // Airdrop 10 MBC tokens to the new member
        // New members are always Student
        // Returns an error if the member already exists
        public shared ({ caller }) func registerMember(member : Member) : async Result<(), Text> {
                if (member.role != #Student) return #err("New members are always Student");

                switch (members.get(caller)) {
                        case (?_) return #err("Member already exists!");
                        case null {
                                let newMember = {
                                        name = member.name;
                                        role = member.role;
                                };
                                switch (await tokenCanister.mint(caller, 10)) {
                                        case (#err(e)) return #err(e);
                                        case (#ok(_)) {};
                                };
                                members.put(caller, newMember);
                                return #ok();
                        };
                };
        };

        // Get the member with the given principal
        // Returns an error if the member does not exist
        public query func getMember(p : Principal) : async Result<Member, Text> {
                switch (members.get(p)) {
                        case null return #err("Member are not exists!");
                        case (?member) return #ok(member);
                };
        };

        // Graduate the student with the given principal
        // Returns an error if the student does not exist or is not a student
        // Returns an error if the caller is not a mentor
        public shared ({ caller }) func graduate(student : Principal) : async Result<(), Text> {
                switch (members.get(caller)) {
                        case (?{ role = #Mentor }) {};
                        case _ { return #err("Caller is not a mentor!") };
                };

                switch (members.get(student)) {
                        case (?member) {
                                switch (member.role) {
                                        case (#Student) {
                                                let updetedMember = {
                                                        name = member.name;
                                                        role = #Graduate;
                                                };
                                                members.put(student, updetedMember);
                                                return #ok();
                                        };
                                        case _ {
                                                return #err("Caller is not a mentor!");
                                        };
                                };
                        };
                        case _ {
                                return #err("Student does not exist or is not a student!");
                        };
                };
        };

        // Create a new proposal and returns its id
        // Returns an error if the caller is not a mentor or doesn't own at least 1 MBC token
        public shared ({ caller }) func createProposal(content : ProposalContent) : async Result<ProposalId, Text> {
                switch (members.get(caller)) {
                        case (?{ role = #Mentor }) {};
                        case _ {
                                return #err("Caller is not a mentor!");
                        };
                };
                let amount = await tokenCanister.balanceOf(caller);
                if (amount < 1) {
                        return #err("Caller don't have enough tokens!");
                };
                let proposal = {
                        id = nextProposalId;
                        content;
                        creator = caller;
                        created = Time.now();
                        executed = null;
                        votes = [];
                        voteScore = 0;
                        status = #Open;
                };

                switch (await tokenCanister.burn(caller, 1)) {
                        case (#err(e)) return #err(e);
                        case (#ok(_)) {};
                };

                nextProposalId += 1;
                proposals.put(proposal.id, proposal);
                return #ok(proposal.id);
        };

        // Get the proposal with the given id
        // Returns an error if the proposal does not exist
        public query func getProposal(id : ProposalId) : async Result<Proposal, Text> {
                switch (proposals.get(id)) {
                        case null return #err("Proposal are not defined!");
                        case (?proposal) return #ok(proposal);
                };
        };

        // Returns all the proposals
        public query func getAllProposal() : async [Proposal] {
                return Iter.toArray(proposals.vals());
        };

        // Vote for the given proposal
        // Returns an error if the proposal does not exist or the member is not allowed to vote
        public shared ({ caller }) func voteProposal(proposalId : ProposalId, yesOrNo : Bool) : async Result<(), Text> {
                switch (members.get(caller)) {
                        case (?{ role = #Mentor }) {};
                        case (?{ role = #Graduate }) {};
                        case _ {
                                return #err("Caller is not a mentor or graduate!");
                        };
                };

                switch (members.get(caller)) {
                        case (?member) {
                                switch (proposals.get(proposalId)) {
                                        case null return #err("Proposal are not exists!");
                                        case (?proposal) {
                                                if (proposal.status != #Open) {
                                                        return #err("Proposal are not open!");
                                                };

                                                if (_hasVoted(proposal, caller)) {
                                                        return #err("Already has a vote by this caller!");
                                                };

                                                let balance = await tokenCanister.balanceOf(caller);

                                                let multiplierVote = switch (yesOrNo) {
                                                        case (true) { 1 };
                                                        case (false) { -1 };
                                                };

                                                let roleMultiplier = switch (member.role) {
                                                        case (#Mentor) { 5 };
                                                        case (#Graduate) { 1 };
                                                        case (#Student) { 0 };
                                                };

                                                let newVoteScore = proposal.voteScore + multiplierVote * roleMultiplier * balance;
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
                                                                await _executeProposal(proposal.content);
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
                        case null return #err("Member are not exists!");
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

        func _executeProposal(content : ProposalContent) : async () {
                switch (content) {
                        case (#ChangeManifesto(newManifesto)) {
                                switch (await webpageCanister.setManifesto(newManifesto)) {
                                        case (#err(_)) {};
                                        case (#ok(_)) {
                                                manifesto := newManifesto;
                                        };
                                };
                        };
                        case (#AddGoal(newGoal)) {
                                goals.add(newGoal);
                        };
                        case (#AddMentor(newMentor)) {
                                switch (members.get(newMentor)) {
                                        case null return ();
                                        case (?member) {
                                                let mentor = {
                                                        name = member.name;
                                                        role = #Mentor;
                                                };
                                                members.put(newMentor, mentor);
                                                return ();
                                        };
                                };
                        };
                };
                return ();
        };

        // Returns the Principal ID of the Webpage canister associated with this DAO canister
        public query func getIdWebpage() : async Principal {
                return canisterIdWebpage;
        };

};
