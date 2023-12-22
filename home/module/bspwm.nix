{ config, lib, pkgs, ... }:
let
  cfg = config.extra.bspwm;
in
{
  options.extra.bspwm.enable = lib.mkEnableOption "Enable bspwm module";

  config = lib.mkIf cfg.enable {
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
