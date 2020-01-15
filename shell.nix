{ reflex-platform ? import ./deps/reflex-platform {}}:
let
  inherit (reflex-platform) nixpkgs;
  inherit (nixpkgs) pkgs;
  drv = import ./. { inherit reflex-platform; };
in
  if pkgs.lib.inNixShell then drv.env else drv
