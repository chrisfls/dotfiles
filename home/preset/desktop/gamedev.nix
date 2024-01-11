{ config, lib, pkgs, specialArgs, ... }:
let
  inherit (config.preset) desktop gamedev;
  enable = desktop && gamedev;
in
{
  options.preset.gamedev = lib.mkEnableOption "Enable gamedev preset";

  config = lib.mkIf enable {
    preset.development = true;

    home.packages = [
      pkgs.aseprite
      pkgs.krita
      pkgs.tiled
      pkgs.lmms
    ];

    nixpkgs.config.allowUnfree = true;
  };
}
