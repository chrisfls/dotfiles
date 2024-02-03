{ config, lib, pkgs, specialArgs, ... }:
let
  inherit (config.presets) desktop gamedev archlinux;
  enable = desktop && gamedev;
in
{
  options.presets.gamedev = lib.mkEnableOption "Enable gamedev preset";

  config = lib.mkIf enable {
    presets.development = true;

    home.packages = lib.mkIf (!archlinux) [
      pkgs.aseprite
      pkgs.gimp
      pkgs.krita
      pkgs.lmms
      pkgs.tiled
    ];

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
