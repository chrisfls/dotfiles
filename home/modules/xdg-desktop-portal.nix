{ config, lib, pkgs, ... }:
let inherit (config.modules.xdg-desktop-portal) enable; in {
  options.modules.xdg-desktop-portal.enable = lib.mkEnableOption "Enable xdg-desktop-portal module";

  config = lib.mkIf enable {
    home.packages = [
      pkgs.xdg-desktop-portal
      pkgs.xdg-desktop-portal-gtk # see "font rendering in GTK apps on KDE"
      pkgs.libsForQt5.xdg-desktop-portal-kde
      pkgs.libsForQt5.kde-cli-tools
      pkgs.libsForQt5.konqueror
    ];

    home.sessionVariables = {
      XDG_CURRENT_DESKTOP= "KDE";
      DE = "kde";
      # breaks vscode:
      # GTK_USE_PORTAL = 1;
    };

    xdg.configFile = {
      "xdg-desktop-portal/portals.conf".text =
        ''
          [preferred]
          default=kde
        '';

      "systemd/user/xdg-desktop-portal.service.d/override.conf".text =
        ''
          [Service]
          Environment="XDG_CURRENT_DESKTOP=KDE"
        '';
    };
  };
}
