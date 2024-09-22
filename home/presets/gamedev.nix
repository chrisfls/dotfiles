{ config, lib, ... }:
let enable = config.presets.gamedev; in
{
  options.presets.gamedev = lib.mkEnableOption "Enable gamedev preset";

  config = lib.mkIf enable {
    presets = {
      desktop = true;
      development = true;
    };

    modules = {
      vfox.enable = false;
    };

    pacman.packages = [
      # "aur/aseprite-git"
      # "chaotic-aur/lmms-git"
      "chaotic-aur/magicavoxel"
      # "extra/audacity"
      # "extra/blender"
      # "extra/gimp"
      # "extra/goxel"
      # "extra/krita"
      # "extra/musescore"
      # "extra/tiled"
    ];
  };
}
