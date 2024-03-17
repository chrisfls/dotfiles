{ config, lib, pkgs, specialArgs, ... }:
let inherit (config.modules.telegram) enable; in {
  options.modules.telegram.enable = lib.mkEnableOption "Enable telegram module";

  config = lib.mkIf enable {
    pacman.packages = [ "extra/telegram-desktop" ];

    home.packages = [
      (pkgs.writeHostScriptBin "telegram-desktop"
        ''
          unset QT_SCREEN_SCALE_FACTORS
          export QT_AUTO_SCREEN_SCALE_FACTOR="0"
          export QT_SCALE_FACTOR="1"
          exec /usr/bin/telegram-desktop "$@"
        '')
    ];

    modules = {
      i3wm = {
        apps."t" = "org.telegram";
        startup = [ "telegram-desktop -startintray" ];
      };

      sway = {
        apps."t" = "org.telegram";
        startup = [ "telegram-desktop -startintray" ];
      };
    };
  };
}
