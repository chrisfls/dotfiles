{ config, lib, pkgs, specialArgs, ... }:
let
  inherit (config.modules.telegram) enable;
  inherit (pkgs.extra) qt mkIfElse;
  exe = "telegram-desktop";
in
{
  options.modules.telegram.enable = lib.mkEnableOption "Enable telegram module";

  config = lib.mkIf enable {
    home.packages = [ (qt.fixScaling pkgs.telegram-desktop) ];

    xsession.windowManager.i3.config = lib.mkIf config.modules.i3wm.enable {
      modes.apps."t" = "exec --no-startup-id gtk-launch org.telegram; mode default";
      startup = [
        { notification = false; command = "telegram-desktop -startintray"; }
      ];
    };
  };
}
