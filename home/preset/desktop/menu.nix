{ pkgs, ... }:
{
  programs.rofi.enable = true;

  extra.nixGL.overlay.rofi = [ "rofi" ];

  services.sxhkd.keybindings."super + Return" = "rofi -show drun";
}
