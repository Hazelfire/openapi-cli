{ pkgs ? import <nixpkgs> {} }:
let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          semantic-scholar-cli =
            haskellPackagesNew.callPackage ./package.nix { };
          openapi3 =
            haskellPackagesNew.callPackage ./nix/openapi.nix { };
        };
      };
    };
  };

  mypkgs = import <nixpkgs> { inherit config; };
in
mypkgs.haskellPackages.semantic-scholar-cli
