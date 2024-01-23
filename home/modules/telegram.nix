{ config, lib, pkgs, specialArgs, ... }:
let
  inherit (config.modules.telegram) enable;
  inherit (pkgs.extra) qt mkIfElse;
  exe = "telegram-desktop";
in
{
  # TODO: pacman

  options.modules.telegram.enable = lib.mkEnableOption "Enable telegram module";

  config = lib.mkIf enable {
    home.packages = [ (qt.fixScaling pkgs.telegram-desktop) ];

    modules.i3wm = {
      apps."t" = "org.telegram";
      startup = ["telegram-desktop -startintray"];
    };
  };
}
