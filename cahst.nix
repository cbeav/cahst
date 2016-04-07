{ mkDerivation, aeson, base, binary, bytestring, cereal
, data-default, network, parsers, protobuf, random, stdenv, text
, tls, unordered-containers
}:
mkDerivation {
  pname = "cahst";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base binary bytestring cereal data-default network parsers
    protobuf random text tls unordered-containers
  ];
  executableHaskellDepends = [ base ];
  license = stdenv.lib.licenses.asl20;
}
