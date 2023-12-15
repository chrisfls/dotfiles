{ config, lib, pkgs, specialArgs, ... }:
let
  lxqt = pkgs.lxqt;
  xdg-desktop-portal = lxqt.xdg-desktop-portal-lxqt;
  notifications = lxqt.lxqt-notificationd;
  polkit-agent = lxqt.lxqt-policykit;
  gui-sudo = lxqt.lxqt-sudo;
  ssh-askpass = lxqt.lxqt-openssh-askpass;
  file-manager = lxqt.pcmanfm-qt;
  volume-mixer = lxqt.pavucontrol-qt;
  system-monitor = lxqt.qps;
  clipboard-manager = lxqt.qlipper;
  screenshot = lxqt.screengrab;
in
{
  imports = [
    ../nixgl.nix
  ];

  config = {
    home.packages = [
      xdg-desktop-portal
      notifications
      polkit-agent
      gui-sudo
      ssh-askpass
      file-manager
      volume-mixer
      system-monitor
      clipboard-manager
      screenshot
    ];

    extra.nixGL.overlay = {
      lxqt = {
        lxqt-sudo = [ "lxqt-sudo" ];
        pavucontrol-qt = [ "pavucontrol-qt" ];
        pcmanfm-qt = [ "pcmanfm-qt" ];
        qlipper = [ "qlipper" ];
        qps = [ "qps" ];
        screengrab = [ "screengrab" ];
      };
    };
  };
}
