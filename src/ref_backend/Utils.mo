//importa los tipos del ICRC1
import TypesICRC1 "../icrc1/Types";

module Utils
{
      // Function to handle minting errors
    public func handleMintError(token: Text, error: TypesICRC1.TransferError): Text {
        switch (error) {
            case (#Duplicate(_d)) {
                return "{\"token\":\"" # token # "\", \"error\":true, \"message\":\"transaction failed: " # token # " mint failed: Duplicate\"}";
            };
            case (#GenericError(_g)) {
                return "{\"token\":\"" # token # "\", \"error\":true, \"message\":\"transaction failed: " # token # " mint failed: GenericError: " # _g.message # "\"}";
            };
            case (#CreatedInFuture(_cif)) {
                return "{\"token\":\"" # token # "\", \"error\":true, \"message\":\"transaction failed: " # token # " mint failed: CreatedInFuture\"}";
            };
            case (#BadFee(_bf)) {
                return "{\"token\":\"" # token # "\", \"error\":true, \"message\":\"transaction failed: " # token # " mint failed: BadFee\"}";
            };
            case (#BadBurn(_bb)) {
                return "{\"token\":\"" # token # "\", \"error\":true, \"message\":\"transaction failed: " # token # " mint failed: BadBurn\"}";
            };
            case (_) {
                return "{\"token\":\"" # token # "\", \"error\":true, \"message\":\"transaction failed: " # token # " mint failed: Other error\"}";
            };
        }
    };
}