{ config, lib, pkgs, specialArgs, ... }:
let inherit (config.modules.obs) enable; in
{
  options.modules.obs.enable = lib.mkEnableOption "Enable obs module";

  config = lib.mkIf enable {
    # REVIEW: do not move to flatpak yet
    pacman.packages = [
      "chaotic-aur/obs-studio-tytan652"
      "chaotic-aur/obs-vkcapture-git"
      "chaotic-aur/vlc-luajit"
    ];
  };
}
