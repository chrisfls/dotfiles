{ config, lib, pkgs, specialArgs, ... }:
let
  inherit (config.presets) desktop gamedev;
  enable = desktop && gamedev;
in
{
  options.presets.gamedev = lib.mkEnableOption "Enable gamedev preset";

  config = lib.mkIf enable {
    pacman.packages = [
      "aur/aseprite"
      "chaotic-aur/lmms-git"
      "extra/tiled"
      "extra/krita"
      "extra/gimp"
      "extra/audacity"
    ];

    presets.development = true;

    modules.obs.enable = true;

    nixpkgs.config.allowUnfree = true;
  };
}
