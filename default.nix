{ reflex-platform ? import ./deps/reflex-platform {} }:
let
  inherit (reflex-platform) nixpkgs ghc;
  drv = nixpkgs.pkgs.haskell.lib.dontCheck (ghc.callCabal2nix "reflex-host-examples" ./. {});
in
  drv

