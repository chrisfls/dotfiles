# TODO: configure
{ config, lib, pkgs, ... }:
let
  cfg = config.extra.betterlockscreen;
in
{
  options.extra.betterlockscreen.enable = lib.mkEnableOption "Enable betterlockscreen module";

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.betterlockscreen ];
  };
}
