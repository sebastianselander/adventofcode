{
  description = "Advent of code";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        haskell-packages = nixpkgs.legacyPackages.${system}.haskell.packages;
        ghcVersion = "966";
        pkgs = import nixpkgs { inherit system; };
      in {
        packages = {
          default =
            haskell-packages.${ghcVersion}.developPackage { root = ./.; };
        };
        devShells = {
          default = pkgs.mkShell {
            nativeBuildInputs = with pkgs; [
              ghc
              haskell-language-server
              haskellPackages.cabal-install
              haskellPackages.fourmolu
              haskellPackages.ghcid
              haskellPackages.hoogle
              zlib
              blas
              lapack
              lua
              (python3.withPackages (p: with p; [ networkx ]))
              pypy310
            ];
          };
        };
      });
}
