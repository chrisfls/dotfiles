{ config, lib, pkgs, specialArgs, ... }:
let inherit (config.modules.steam) enable; in {
  options.modules.steam.enable = lib.mkEnableOption "Enable steam module";

  config = lib.mkIf enable {
    pacman.packages = [ "multilib/steam" ];
  };
}
