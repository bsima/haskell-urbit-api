{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc884" }:

nixpkgs.mkShell {
  name = "urbit-airlock-shell";
  buildInputs = [
    nixpkgs.ormolu.bin
    (nixpkgs.pkgs.haskell.packages.${compiler}.ghcWithPackages (hp: with hp; [
      aeson base bytestring lens text wai wai-extra wreq
    ]))
  ];
}
