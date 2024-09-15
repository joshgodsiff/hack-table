{
  description = "A Haskell implementation of the Kademlia distributed hash table";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        haskellPackages = pkgs.haskellPackages;

        packageName = "kademlia";

        package = haskellPackages.callCabal2nix packageName self {};

      in {
        packages.${packageName} = package;
        defaultPackage = self.packages.${system}.${packageName};

        devShell = pkgs.mkShell {
          buildInputs = with haskellPackages; [
            ghc
            cabal-install
            haskell-language-server
            hlint
            hoogle
            package
          ];
        };
      }
    );
}