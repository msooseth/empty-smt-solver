{
  description = "A simple Haskell binary: empty-smt-solver";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        haskellPackages = pkgs.haskellPackages;
      in
      {
        packages.default = haskellPackages.callCabal2nix "empty-smt-solver" ./. { };

        apps.default = {
          type = "app";
          program = "${self.packages.${system}.default}/bin/empty-smt-solver";
        };

        devShells.default = haskellPackages.shellFor {
          packages = p: [ self.packages.${system}.default ];
          buildInputs = with haskellPackages; [
            cabal-install
            ghcid
            haskell-language-server
          ];
        };
      });
}
