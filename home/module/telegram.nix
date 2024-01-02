{ config, lib, pkgs, specialArgs, ... }:
let
  inherit (config.module.telegram) enable;
  pkg = specialArgs.qt.fixScaling { package = pkgs.telegram-desktop; };
in
{
  options.module.telegram.enable = lib.mkEnableOption "Enable telegram module";

  config = lib.mkIf enable {
    home.packages = [ pkg ];

    xsession.windowManager.i3.config = lib.mkIf config.i3wm.enable {
      modes.apps."t" = "exec gtk-launch org.telegram";
      startup = [
        { notification = false; command = "${pkgs.telegram-desktop}/bin/telegram-desktop -startintray"; }
      ];
    };
  };
}
