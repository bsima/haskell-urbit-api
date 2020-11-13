let
  depsOverlay = self: super: {
    haskell = super.haskell // {
      packages = super.haskell.packages // {
        ghc884 = super.haskell.packages.ghc884.override (old: {
          overrides = with super.pkgs.haskell.lib; haskellSelf: haskellSuper: {
            req-conduit = dontCheck haskellSuper.req-conduit; # wants network for tests
          };
        });
      };
    };
  };
  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/13209156c191524437d5556bd8b24a132c4a899a.tar.gz";
    sha256 = "06g21xf0nd0r2nziw72qk3vp8cd3vdy39snd44fir6j3fyjx1p98";
   };
in
import nixpkgs {
  overlays = [
    depsOverlay
  ];
}
