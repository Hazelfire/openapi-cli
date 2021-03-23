{ mkDerivation, aeson, base, bytestring, directory, hpack
, http-client, http-client-tls, insert-ordered-containers
, json-pointy, lens, lib, openapi3, process, text, typed-process
, uri-encode, yaml
}:
mkDerivation {
  pname = "openapi-generic-cli";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring directory http-client http-client-tls
    insert-ordered-containers json-pointy lens openapi3 process text
    typed-process uri-encode yaml
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson base bytestring directory http-client http-client-tls
    insert-ordered-containers json-pointy lens openapi3 process text
    typed-process uri-encode yaml
  ];
  testHaskellDepends = [
    aeson base bytestring directory http-client http-client-tls
    insert-ordered-containers json-pointy lens openapi3 process text
    typed-process uri-encode yaml
  ];
  prePatch = "hpack";
  homepage = "https://github.com/Hazelfire/openapi-generic-cli#readme";
  license = lib.licenses.bsd3;
}
