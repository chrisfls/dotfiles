{ config, lib, pkgs, ... }:
let
  cfg = config.extra.window-manager;

  xorg = pkgs.xorg;

  i3 = config.xsession.windowManager.i3;

  mod = config.xsession.windowManager.i3.config.modifier;
in
{
  options.extra.window-manager.enable = lib.mkEnableOption "Enable window-manager module";

  config = lib.mkIf cfg.enable {
    home.packages = [
      pkgs.picom
    ];

    xsession.windowManager.bspwm = {
      enable = true;
      monitors = {
        HDMI-1 = [ "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" ];
      };
      rules = { };
      settings = {
        border_width = 2;
        gapless_monocle = true;
      };
    };
  };
}
