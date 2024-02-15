{ config, lib, pkgs, specialArgs, ... }:
let
  inherit (config.presets) desktop gamedev archlinux;
  enable = desktop && gamedev;
in
{
  options.presets.gamedev = lib.mkEnableOption "Enable gamedev preset";

  config = lib.mkIf enable {
    presets.development = true;

    modules.obs.enable = true;

    pacman.packages = [
      "aur/aseprite"
      "extra/gimp"
      "extra/krita"
      "chaotic-aur/lmms-git"
      "extra/tiled"
    ];

    nixpkgs.config.allowUnfree = true;
  };
}
