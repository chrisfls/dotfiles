{ config, lib, pkgs, ... }:
let
  cfg = config.module.xdg-portal;
in
{
  options.module.xdg-portal.enable = lib.mkEnableOption "Enable xdg-portal module";
  # TODO: review this
  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.lxqt.xdg-desktop-portal-lxqt ];
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