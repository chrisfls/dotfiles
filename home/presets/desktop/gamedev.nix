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
      "extra/gimp"
      "extra/krita"
      "chaotic-aur/lmms-git"
      "extra/tiled"
    ];

    presets.development = true;

    modules.obs.enable = true;

    nixpkgs.config.allowUnfree = true;
  };
}
