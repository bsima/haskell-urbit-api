{ nixpkgs ? import ./nixpkgs.nix
, compiler ? "ghc884"
}:
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./urbit-api.nix { }
