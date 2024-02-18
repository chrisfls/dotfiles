{ config, lib, pkgs, ... }:
let inherit (config.modules.onedrive) enable; in
{
  options.modules.onedrive.enable = lib.mkEnableOption "Enable onedrive module";
  config = lib.mkIf enable {
    pacman.packages = [ "chaotic-aur/onedriver" ];
  };
}
