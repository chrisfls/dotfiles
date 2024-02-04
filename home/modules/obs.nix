{ config, lib, pkgs, specialArgs, ... }:
let
  inherit (config.presets) archlinux;
  inherit (config.modules.obs) enable;
in
{
  options.modules.obs.enable = lib.mkEnableOption "Enable obs module";

  config = lib.mkIf enable {
    # TODO: nix obs
    pacman.packages = [ "chaotic-aur/obs-studio-tytan652" ];
  };
}
