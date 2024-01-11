{ config, lib, pkgs, specialArgs, ... }:
let
  inherit (config.presets) desktop gamedev;
  enable = desktop && gamedev;
in
{
  options.presets.gamedev = lib.mkEnableOption "Enable gamedev preset";

  config = lib.mkIf enable {
    presets.development = true;

    home.packages = [
      pkgs.aseprite
      pkgs.krita
      pkgs.tiled
      pkgs.lmms
    ];

    nixpkgs.config.allowUnfree = true;
  };
}
