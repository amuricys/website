{
  description = "My Halogen Website";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    # flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs }: {
    devShell.x86_64-darwin = let
      pkgs = nixpkgs.legacyPackages.x86_64-darwin;
    in pkgs.mkShell {
      buildInputs = with pkgs; [
        nodejs-16_x
        purescript
        spago
        nodePackages.purescript-language-server
        # nodePackages.parcel something is broken about nixpkgs' parcel 2.8.3, so we'll stick to package.json
        # nodePackages.halogen
      ];
    };
  };
}