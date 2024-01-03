{ config, lib, pkgs, ... }:
let inherit (config.module.xdg-desktop-portal) enable; in {
  options.module.xdg-desktop-portal.enable = lib.mkEnableOption "Enable xdg-desktop-portal module";

  config = lib.mkIf enable {
    home.packages = [
      pkgs.xdg-desktop-portal
      pkgs.lxqt.xdg-desktop-portal-lxqt
    ];

    pacman.overrides = {
      xdg-desktop-portal = [ "extra/xdg-desktop-portal" ];
      lxqt.xdg-desktop-portal-lxqt = [ "extra/xdg-desktop-portal-lxqt" ];
    };

    home.sessionVariables.GTK_USE_PORTAL = 1;

    xdg.configFile = {
      "xdg-desktop-portal/portals.conf".text =
        ''
          [preferred]
          default=lxqt
          org.freedesktop.impl.portal.FileChooser=lxqt
        '';

      "systemd/user/xdg-desktop-portal.service.d/override.conf".text =
        ''
          [Service]
          Environment="XDG_CURRENT_DESKTOP=LXQt"
        '';
    };
  };
}
