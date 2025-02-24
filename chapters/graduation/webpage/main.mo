import Types "types";
import Result "mo:base/Result";
import Text "mo:base/Text";
import Principal "mo:base/Principal";
actor Webpage {

    type Result<A, B> = Result.Result<A, B>;
    type HttpRequest = Types.HttpRequest;
    type HttpResponse = Types.HttpResponse;

    stable let canisterIdDAO : Principal = Principal.fromText("wch4f-miaaa-aaaab-qadfa-cai");

    // The manifesto stored in the webpage canister should always be the same as the one stored in the DAO canister
    stable var manifesto : Text = "Let's graduate!";

    func _getWebpage() : Text {
        var webpage = "<style>" #
        "body { text-align: center; font-family: Arial, sans-serif; background-color: #f0f8ff; color: #333; }" #
        "em { font-style: italic; display: block; margin-bottom: 20px; }" #
        "</style>";

        webpage := webpage # "<em>" # manifesto # "</em>";
        return webpage;
    };

    // The webpage displays the manifesto
    public func http_request(request : HttpRequest) : async HttpResponse {
        return ({
            headers = [("Content-Type", "text/html; charset=UTF-8")];
            status_code = 200 : Nat16;
            body = Text.encodeUtf8(_getWebpage());
            streaming_strategy = null;
        });
    };

    // This function should only be callable by the DAO canister (no one else should be able to change the manifesto)
    public shared ({ caller }) func setManifesto(newManifesto : Text) : async Result<(), Text> {
        if (caller != canisterIdDAO) {
            return #err("Caller are not DAO canister!");
        };
        manifesto := newManifesto;
        return #ok();
    };
};
