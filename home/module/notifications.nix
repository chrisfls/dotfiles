# TODO: **ONE DAY** replace with dunst or mako
{ config, lib, pkgs, ... }:
let
  cfg = config.extra.notifications;
in
{
  options.extra.notifications.enable = lib.mkEnableOption "Enable notifications module";

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.lxqt.lxqt-notificationd ];

    xsession.windowManager.bspwm.startupPrograms = [ "lxqt-notificationd" ];
  };
}
