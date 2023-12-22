{ config, lib, pkgs, ... }:
let
  cfg = config.extra.bspwm;
in
{
  options.extra.bspwm.enable = lib.mkEnableOption "Enable bspwm module";

  config = lib.mkIf cfg.enable {
    home.sessionVariables.DESKTOP_SESSION = "bspwm";

    xsession.initExtra = "systemctl --user import-environment DESKTOP_SESSION";

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
