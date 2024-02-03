{ config, lib, pkgs, specialArgs, ... }:
let
  inherit (config.modules.steam) enable;
  inherit (pkgs.extra) qt mkIfElse;
in
{
  options.modules.steam.enable = lib.mkEnableOption "Enable steam module";

  config = lib.mkIf enable {
    pacman.packages = [ "multilib/steam" ];
  };
}
