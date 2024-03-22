{ config, lib, pkgs, ... }:
let inherit (config.modules.xdg) enable; in {
  options.modules.xdg.enable = lib.mkEnableOption "Enable xdg module";

  config = lib.mkIf enable {
    pacman.packages = [
      "extra/xdg-desktop-portal-kde"
      "extra/xdg-desktop-portal-gtk"
      "extra/xdg-user-dirs"
      "extra/xdg-utils"
    ];

    home.sessionVariables = {
      XDG_CURRENT_DESKTOP = "kde";
      XDG_SESSION_TYPE = "wayland";
      # GTK_USE_PORTAL = 1; # breaks vscode
    };

    xdg = {
      enable = true;
      userDirs.enable = true;
      configFile = {
        "xdg-desktop-portal/portals.conf".text =
          ''
            [preferred]
            default=kde
          '';

        "systemd/user/xdg-desktop-portal.service.d/override.conf".text =
          ''
            [Service]
            Environment="XDG_CURRENT_DESKTOP=kde"
          '';
      };
      desktopEntries = {
        "avahi-discover" = { name = "Avahi Zeroconf Browser"; noDisplay = true; };
        "bssh" = { name = "Avahi SSH Server Browser"; noDisplay = true; };
        "bvnc" = { name = "Avahi VNC Server Browser"; noDisplay = true; };
        "fish" = { name = "fish"; noDisplay = true; };
        "micro" = {
          name = "Micro";
          genericName = "Text Editor";
          comment = "Edit text files in a terminal";
          icon = "micro";
          type = "Application";
          categories = [ "Utility" "TextEditor" "Development" ];
          exec = "micro %F";
          startupNotify = true;
          terminal = true;
          noDisplay = false;
          mimeType = [
            "text/plain"
            "text/x-chdr"
            "text/x-csrc"
            "text/x-c++hdr"
            "text/x-c++src"
            "text/x-java"
            "text/x-dsrc"
            "text/x-pascal"
            "text/x-perl"
            "text/x-python"
            "application/x-php"
            "application/x-httpd-php3"
            "application/x-httpd-php4"
            "application/x-httpd-php5"
            "application/xml"
            "text/html"
            "text/css"
            "text/x-sql"
            "text/x-diff"
          ];
        };
      };
    };
  };
}
