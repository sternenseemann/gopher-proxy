{ mkDerivation, attoparsec, base, bytestring, directory, errors
, http-types, lucid, mime-types, network, optparse-applicative
, stdenv, text, wai, warp
}:
mkDerivation {
  pname = "gopher-proxy";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    attoparsec base bytestring directory errors http-types lucid
    mime-types network optparse-applicative text wai warp
  ];
  description = "proxy gopher over http";
  license = stdenv.lib.licenses.gpl3;
}
