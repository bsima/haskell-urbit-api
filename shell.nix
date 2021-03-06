{ nixpkgs ? import ./nixpkgs.nix
, compiler ? "ghc884"
}:

nixpkgs.mkShell {
  name = "urbit-api-shell";
  buildInputs = [
    nixpkgs.ormolu.bin

    (nixpkgs.pkgs.haskell.packages.${compiler}.ghcWithPackages (hp: with hp; [

    aeson base bytestring conduit conduit-extra http-client modern-uri
    req req-conduit text uuid

    ]))
  ];
}
