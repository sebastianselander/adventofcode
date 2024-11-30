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
        ghcVersion = "910";
        pkgs = import nixpkgs { inherit system; };
      in {
        packages = {
          default =
            haskell-packages.${ghcVersion}.developPackage { root = ./.; };
        };
        devShells = {
          default = pkgs.mkShell {
            nativeBuildInputs = with pkgs; [
              haskell.compiler.ghc910
              (haskell-language-server.override {
                supportedGhcVersions = [ "${ghcVersion}" ];
              })
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
