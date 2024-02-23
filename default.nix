let
  revision = "442d407992384ed9c0e6d352de75b69079904e4e";
  sha256 = "0rbaxymznpr2gfl5a9jyii5nlpjc9k2lrwlw2h5ccinds58c202k";
  nixpkgs = import (fetchTarball {
    name = "nixpkgs";
    url = "https://github.com/NixOS/nixpkgs/archive/${revision}.tar.gz";
    inherit sha256;
  }) { };

in { pkgs ? nixpkgs }:

let
  inherit (pkgs) haskellPackages lib pre-commit yamlfmt;

  package = haskellPackages.developPackage { root = ./.; };

  devShell = package.overrideAttrs (prevAttrs: {
    name = "dev-shell-for-${builtins.baseNameOf ./.}";
    nativeBuildInputs = (prevAttrs.nativeBuildInputs or [ ])
      ++ (with haskellPackages; [
        cabal-fmt
        cabal-install
        fourmolu
        haskell-language-server
        hlint
        pre-commit
        yamlfmt
      ]);
    shellHook = "pre-commit install -f";
  });

in if lib.inNixShell then devShell else package
