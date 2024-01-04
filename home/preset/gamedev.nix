{ config, lib, pkgs, ... }:
let
  enable = config.preset.gamedev;
in
{
  options.preset.gamedev = lib.mkEnableOption "Enable gamedev preset";

  config = (lib.mkIf enable {
    preset.development = true;

    home.packages = [
      pkgs.usr.aseprite
      pkgs.usr.krita
      pkgs.usr.tiled
      pkgs.usr.lmms
    ];

    pacman.overrides = {
      aseprite = [ "aur/aseprite" ];
      krita = [ "extra/krita" ];
      tiled = [ "extra/tiled" ];
      lmms = [ "extra/lmms" ];
    };

    nixpkgs.config.allowUnfree = true;
  });
}
