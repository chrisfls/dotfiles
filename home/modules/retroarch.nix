{ config, lib, pkgs, ... }:
let inherit (config.modules.retroarch) enable; in
{
  options.modules.retroarch.enable = lib.mkEnableOption "Enable retroarch module";
  config = lib.mkIf enable {
    # TODO: move to flatpak
    pacman.packages = [
      # "chaotic-aur/libretro-mupen64plus-next-git" # extra/libretro-mupen64plus-next
      "extra/mesa-utils"
    ];
  };
}
