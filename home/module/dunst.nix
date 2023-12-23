# TODO: **ONE DAY** replace with dunst or mako
{ config, lib, pkgs, ... }:
let
  cfg = config.module.dunst;
in
{
  options.module.dunst.enable = lib.mkEnableOption "Enable dunst module";

  config = lib.mkIf cfg.enable {
    # home.packages = [ pkgs.lxqt.lxqt-notificationd ];
    # xsession.windowManager.bspwm.startupPrograms = [ "lxqt-notificationd" ];
  };
}
