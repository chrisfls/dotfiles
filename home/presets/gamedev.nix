{ config, lib, ... }:
let enable = config.presets.gamedev; in
{
  options.presets.gamedev = lib.mkEnableOption "Enable gamedev preset";

  config = lib.mkIf enable {
    presets = {
      desktop = true;
      development = true;
    };

    modules.vfox.enable = true;

    pacman.packages = [
      "aur/aseprite"
      "chaotic-aur/lmms-git"
      "extra/audacity"
      "extra/gimp"
      "extra/go-task"
      "extra/krita"
      "extra/tiled"
    ];
  };
}
