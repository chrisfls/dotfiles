{ config, lib, pkgs, specialArgs, ... }:
let inherit (config.modules.telegram) enable; in {
  options.modules.telegram.enable = lib.mkEnableOption "Enable telegram module";

  config = lib.mkIf enable {
    pacman.packages = [ "extra/telegram-desktop" ];

    modules.sway = {
      apps."t" = "org.telegram";
      startup = [ "telegram-desktop -startintray" ];
    };
  };
}
