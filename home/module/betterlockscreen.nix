# TODO: configure
{ config, lib, pkgs, ... }:
let
  cfg = config.module.betterlockscreen;
in
{
  options.module.betterlockscreen.enable = lib.mkEnableOption "Enable betterlockscreen module";

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.betterlockscreen ];
  };
}
