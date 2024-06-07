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
      vfox.enable = true;
      fish.aliases."task" = "go-task";
    };

    pacman.packages = [
      "aur/aseprite"
      "chaotic-aur/lmms-git"
      "chaotic-aur/magicavoxel"
      "extra/audacity"
      "extra/blender"
      "extra/gimp"
      "extra/go-task"
      "extra/goxel"
      "extra/krita"
      "extra/tiled"
    ];
  };
}
