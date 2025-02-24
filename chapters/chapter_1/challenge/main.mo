import Buffer "mo:base/Buffer";
actor {

    let name : Text = "Learn Finance";

    var manifesto : Text = "Everyone deserves access to financial knowledge. Understanding money, investments, and economic systems should not be limited to a select few but available to all who seek it.";

    var goals : Buffer.Buffer<Text> = Buffer.Buffer<Text>(2);

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
        return Buffer.toArray<Text>(goals);
    };
};