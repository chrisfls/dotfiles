{ config, lib, pkgs, specialArgs, ... }:
let
  inherit (config.module.steam) enable;
  inherit (specialArgs) mkIfElse;
in
{
  options.module.steam.enable = lib.mkEnableOption "Enable steam module";

  config = lib.mkIf enable (mkIfElse config.preset.non-nixos
    { pacman.packages = [ "multilib/steam" ]; }
    # probably steam works better when installed system wide
    { home.packages = [ pkgs.steam ]; });
}
