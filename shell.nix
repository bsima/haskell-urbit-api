{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc884" }:

nixpkgs.mkShell {
  name = "urbit-airlock-shell";
  buildInputs = [
    nixpkgs.ormolu.bin
    (nixpkgs.pkgs.haskell.packages.${compiler}.ghcWithPackages (hp: with hp; [
      aeson base bytestring conduit conduit-extra http-client lens
      modern-uri req req-conduit text uuid wai wai-extra
    ]))
  ];
}
