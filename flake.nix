{
  description = "Advent of code";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in {
        devShells = {
          default = pkgs.mkShell {
            nativeBuildInputs = with pkgs; [
              haskell.compiler.ghc912
              (haskell-language-server.override {supportedGhcVersions = [ "912"];})
              haskellPackages.cabal-install
              haskellPackages.fourmolu
              haskellPackages.ghcid
              haskellPackages.hoogle
              zlib
              blas
              lapack
            ];
          };
        };
      });
}
