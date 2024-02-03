{ config, lib, pkgs, specialArgs, ... }:
let
  inherit (config.modules.telegram) enable;
  inherit (pkgs.extra) qt mkIfElse;

  pkg =
    if config.presets.archlinux then
      pkgs.writeShellScriptBin "telegram-desktop"
        ''
          unset QT_SCREEN_SCALE_FACTORS
          export QT_AUTO_SCREEN_SCALE_FACTOR="0"
          export QT_SCALE_FACTOR="1"
          exec /usr/bin/telegram-desktop "$@"
        ''

    else
      qt.fixScaling pkgs.telegram-desktop;
in
{
  options.modules.telegram.enable = lib.mkEnableOption "Enable telegram module";

  config = lib.mkIf enable {
    home.packages = [ pkg ];

    pacman.packages = [ "extra/telegram-desktop" ];

    modules.i3wm = {
      apps."t" = "org.telegram";
      startup = [ "telegram-desktop -startintray" ];
    };
  };
}
