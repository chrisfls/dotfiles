{ pkgs, ... }:
{
  home.packages = [
    pkgs.contour
  ];

  extra.nixGL.overlay.contour = [ "contour" ];

  services.sxhkd.keybindings."super + semicolon" = "contour";
}
