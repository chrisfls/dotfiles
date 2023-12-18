# TODO: replace with dunst or mako
{ pkgs, ... }:
{
  home.packages = [ pkgs.lxqt.lxqt-notificationd ];

  extra.nixGL.overlay.lxqt.lxqt-notificationd = [ "lxqt-notificationd" ];

  xsession.windowManager.bspwm.startupPrograms = [ "lxqt-notificationd" ];
}
