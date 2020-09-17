{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc884" }:
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./urbit-airlock.nix { }
