{ pkgs, ... }:
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
      #notifications
      #polkit-agent
      #gui-sudo
      #ssh-askpass
      file-manager
      #volume-mixer
      #system-monitor
      #clipboard-manager
      #screenshot
    ];

    extra.nixGL.wrap = {
      #"${notifications}".enable = true;
      #"${polkit-agent}".enable = true;
      #"${gui-sudo}".enable = true;
      #"${ssh-askpass}".enable = true;
      "lxqt.pcmanfm-qt" = {
        enable = true;
        targets = [ "pcmanfm-qt" ];
      };
      #"${volume-mixer}".enable = true;
      #"${system-monitor}".enable = true;
      #"${clipboard-manager}".enable = true;
      #"${screenshot}".enable = true;
    };
  };
}
/*
  nixpkgs.overlays = [
      (self: super: {
        lxqt.xdg-desktop-portal-lxqt = wrapGL super.lxqt.xdg-desktop-portal-lxqt;
        lxqt.lxqt-notificationd = wrapGL super.lxqt.lxqt-notificationd;
        lxqt.lxqt-policykit = wrapGL super.lxqt.lxqt-policykit;
        lxqt.lxqt-sudo = wrapGL super.lxqt.lxqt-sudo;
        lxqt.lxqt-openssh-askpass = wrapGL super.lxqt.lxqt-openssh-askpass;
        lxqt.pcmanfm-qt = wrapGL super.lxqt.pcmanfm-qt;
        lxqt.pavucontrol-qt = wrapGL super.lxqt.pavucontrol-qt;
        lxqt.qps = wrapGL super.lxqt.qps;
        lxqt.qlipper = wrapGL super.lxqt.qlipper;
        lxqt.screengrab = wrapGL super.lxqt.screengrab;
      })*/
