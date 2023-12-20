{ config, lib, pkgs, ... }:
let
  cfg = config.extra.menu;
in
{
  options.extra.menu.enable = lib.mkEnableOption "Enable menu module";

  config = lib.mkIf cfg.enable {
    programs.rofi.enable = true;

    services.sxhkd.keybindings."super + Return" = "rofi -show drun";
  };
}
