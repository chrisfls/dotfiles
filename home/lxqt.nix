{ config, lib, pkgs, ... }:
let
  lxqt = pkgs.lxqt;
in
{
  config = {
    home.packages = [
      # shell
      lxqt.xdg-desktop-portal-lxqt
      lxqt.lxqt-sudo
      lxqt.lxqt-notificationd   # TODO: replace with dunst or mako
      lxqt.screengrab           # TODO: check better alternative
      lxqt.lxqt-policykit       # TODO: check if it is needed
      lxqt.lxqt-openssh-askpass # TODO: check if it's worth to replace
                                # with ksshaskpass or something else

      # applications
      lxqt.pcmanfm-qt
      lxqt.lxqt-archiver
      lxqt.pavucontrol-qt
      lxqt.qlipper
      lxqt.qps
    ];
  };
}
