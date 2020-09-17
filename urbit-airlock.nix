{ mkDerivation, aeson, base, bytestring, lens, stdenv, text, wai
, wai-extra, wreq
}:
mkDerivation {
  pname = "urbit-airlock";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring lens text wai wai-extra wreq
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base ];
  homepage = "https://github.com/bsima/urbit-airlock";
  license = stdenv.lib.licenses.bsd3;
}
