{ mkDerivation, aeson, base, bytestring, hpack, http-client
, http-client-tls, lens, lib, openapi3, text, yaml
}:
mkDerivation {
  pname = "semantic-scholar-cli";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring http-client http-client-tls lens openapi3
    text yaml
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson base bytestring http-client http-client-tls lens openapi3
    text yaml
  ];
  testHaskellDepends = [
    aeson base bytestring http-client http-client-tls lens openapi3
    text yaml
  ];
  prePatch = "hpack";
  homepage = "https://github.com/githubuser/semantic-scholar-cli#readme";
  license = lib.licenses.bsd3;
}
