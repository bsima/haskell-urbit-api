{ mkDerivation, aeson, base, bytestring, conduit, conduit-extra
, http-client, modern-uri, req, req-conduit, stdenv, text, uuid
}:
mkDerivation {
  pname = "urbit-airlock";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring conduit conduit-extra http-client modern-uri
    req req-conduit text uuid
  ];
  homepage = "https://github.com/bsima/urbit-airlock";
  description = "Talk to Urbit from Haskell";
  license = stdenv.lib.licenses.bsd3;
}
