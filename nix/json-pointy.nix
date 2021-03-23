{ mkDerivation, aeson, attoparsec, base, bytestring, containers
, directory, doctest, filepath, hashable, http-types, lib
, microlens, mtl, tasty, tasty-discover, tasty-hunit
, template-haskell, text, th-lift-instances, unordered-containers
, uri-bytestring, vector, pkgs
}:
let 
  json-pointy-src = 
    pkgs.fetchFromGitHub {
      owner = "Hazelfire";
      repo = "json-utils";
      rev = "e2a22d3ceed1a401aaac4349ef853c42ece2004a";
      sha256 = "09544l7cayycfg0ahdmf3y0k2lzxpcwczbz6pcqp95rxi3xcxvl4";
      fetchSubmodules = true;
    };
in
  mkDerivation {
    pname = "json-pointy";
    version = "0.1.0.1";
    src = "${json-pointy-src}/json-pointy";
    libraryHaskellDepends = [
      aeson attoparsec base bytestring http-types microlens
      template-haskell text th-lift-instances unordered-containers
      uri-bytestring vector
    ];
    testHaskellDepends = [
      base bytestring containers directory doctest filepath hashable mtl
      tasty tasty-discover tasty-hunit text unordered-containers vector
    ];
    testToolDepends = [ tasty-discover ];
    homepage = "https://github.com/iand675/json-utils/";
    description = "JSON Pointer (RFC 6901) parsing, access, and modification";
    license = lib.licenses.bsd3;
  }
