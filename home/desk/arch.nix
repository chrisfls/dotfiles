{ flakes, pkgs, ... }:
let
  overlay = import flakes.nixpkgs {
    system = pkgs.system;
    overlays = [ flakes.nixgl.overlay ];
  };
in
{
  home.packages = [
    overlay.nixgl.nixGLIntel
  ];
}
