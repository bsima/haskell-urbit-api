{ mkDerivation, aeson, base, bytestring, conduit, conduit-extra
, http-client, lens, modern-uri, req, req-conduit, stdenv, text
, uuid, wai, wai-extra
}:
mkDerivation {
  pname = "urbit-airlock";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring conduit conduit-extra http-client lens
    modern-uri req req-conduit text uuid wai wai-extra
  ];
  homepage = "https://github.com/bsima/urbit-airlock";
  license = stdenv.lib.licenses.bsd3;
}
