{ config, lib, pkgs, specialArgs, ... }:
let
  inherit (config.module.telegram) enable;

  pkg =

    /*
      # TODO: patch .desktop instead
      specialArgs.qt.fixScaling
        {
          package = pkgs.telegram-desktop;
          exe = "telegram-desktop";
        }
    */
    pkgs.telegram-desktop
  ;
in
{
  options.module.telegram.enable = lib.mkEnableOption "Enable telegram module";

  config = lib.mkIf enable {
    home.packages = [ pkg ];
    # pacman.usr.telegram-desktop = [ "extra/telegram-desktop" ];

    xsession.windowManager.i3.config = lib.mkIf config.module.i3wm.enable {
      modes.apps."t" = "exec gtk-launch org.telegram";
      startup = [
        # TODO: gtk-launch
        { notification = false; command = "${pkg}/bin/telegram-desktop -startintray"; }
      ];
    };
  };
}
