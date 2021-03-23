{ pkgs ? import <nixpkgs> {} }:
let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          openapi-generic-cli =
            haskellPackagesNew.callPackage ./package.nix { };
          openapi3 =
            haskellPackagesNew.callPackage ./nix/openapi.nix { };
          json-pointy =
            haskellPackagesNew.callPackage ./nix/json-pointy.nix { };
        };
      };
    };
  };

  mypkgs = import <nixpkgs> { inherit config; };
in
mypkgs.haskellPackages.openapi-generic-cli
