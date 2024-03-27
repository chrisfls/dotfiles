{ config, lib, ... }:
let enable = config.presets.gamedev; in
{
  options.presets.gamedev = lib.mkEnableOption "Enable gamedev preset";

  config = lib.mkIf enable {
    presets = {
      desktop = true;
      development = true;
    };

    pacman.packages = [
      "aur/aseprite"
      "chaotic-aur/lmms-git"
      "extra/tiled"
      "extra/krita"
      "extra/gimp"
      "extra/audacity"
    ];
  };
}
