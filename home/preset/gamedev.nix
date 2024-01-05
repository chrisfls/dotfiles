{ config, lib, pkgs, ... }:
let
  enable = config.preset.gamedev;
in
{
  options.preset.gamedev = lib.mkEnableOption "Enable gamedev preset";

  config = (lib.mkIf enable {
    preset.development = true;

    home.packages = [
      pkgs.aseprite
      pkgs.krita
      pkgs.tiled
      pkgs.lmms
    ];

    nixpkgs.config.allowUnfree = true;
  });
}
