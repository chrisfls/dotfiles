{ config, lib, pkgs, ... }:
let
  cfg = config.extra;
in
{
  options.extra =
    let
      lxqt = pkgs.lxqt;

      program-with-exe = self: default: {
        enable = lib.mkEnableOption "Program configuration";

        package = lib.mkOption {
          type = lib.types.package;
          default = default;
          description = "Package used";
        };

        exe = lib.mkOption {
          type = lib.types.package;
          default = lib.getEXE self.package;
          description = "Main binary";
        };
      };
    in
    {
      xdg-desktop-portal = program-with-exe cfg.xdg-desktop-portal lxqt.xdg-desktop-portal-lxqt;
      notifications = program-with-exe cfg.notifications lxqt.lxqt-notificationd;
      polkit-agent = program-with-exe cfg.polkit-agent lxqt.lxqt-policykit;
      gui-sudo = program-with-exe cfg.gui-sudo lxqt.lxqt-sudo;
      ssh-askpass = program-with-exe cfg.ssh-askpass lxqt.lxqt-openssh-askpass;
      file-manager = program-with-exe cfg.file-manager lxqt.pcmanfm-qt;
      volume-mixer = program-with-exe cfg.volume-mixer lxqt.pavucontrol-qt;
      system-monitor = program-with-exe cfg.system-monitor lxqt.qps;
      clipboard-manager = program-with-exe cfg.clipboard-manager lxqt.qlipper;
      screenshot = program-with-exe cfg.screenshot lxqt.screengrab;
    };

  config =
    let
      program-config = program:
        lib.mkIf program.enable { home.packages = [ program.package ]; };
    in
    lib.mkMerge [
      (program-config cfg.xdg-desktop-portal)
      (program-config cfg.notifications)
      (program-config cfg.polkit-agent)
      (program-config cfg.gui-sudo)
      (program-config cfg.ssh-askpass)
      (program-config cfg.file-manager)
      (program-config cfg.volume-mixer)
      (program-config cfg.system-monitor)
      (program-config cfg.clipboard-manager)
      (program-config cfg.screenshot)
    ];
}
