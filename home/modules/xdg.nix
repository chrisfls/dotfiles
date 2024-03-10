{ config, lib, pkgs, ... }:
let inherit (config.modules.xdg) enable; in {
  options.modules.xdg.enable = lib.mkEnableOption "Enable xdg module";

  config = lib.mkIf enable {
    pacman.packages = [
      # "extra/xdg-desktop-portal-lxqt"
      # "extra/xdg-desktop-portal" # will be installed for flatpak anyway
      # "extra/xdg-desktop-portal-gtk"
      "extra/xdg-user-dirs"
      "extra/xdg-utils"
    ];

    # home.sessionVariables = {
    #   XDG_CURRENT_DESKTOP = "LXQt";
    #   DE = "lxqt";
    #   # GTK_USE_PORTAL = 1; # breaks vscode
    # };

    xdg = {
      enable = true;
      mimeApps = {
        enable = true;
        defaultApplications = {
          "inode/directory" = "org.kde.dolphin.desktop";
          "text/plain" = "featherpad.desktop";
          "application/zip" = "lxqt-archiver.desktop";
          "application/rar" = "lxqt-archiver.desktop";
          "application/7z" = "lxqt-archiver.desktop";
          "application/*tar" = "lxqt-archiver.desktop";
          "video/*" = "org.kde.haruna.desktop";
          "audio/*" = "org.kde.juk.desktop";
          "x-scheme-handler/tg" = "org.telegram.desktop.desktop";
        };
      };
      userDirs.enable = true;
      # configFile = {
      #   "xdg-desktop-portal/portals.conf".text =
      #     ''
      #       [preferred]
      #       default=gtk
      #       org.freedesktop.impl.portal.FileChooser=xapp
      #     '';

      #   "systemd/user/xdg-desktop-portal.service.d/override.conf".text =
      #     ''
      #       [Service]
      #       Environment="XDG_CURRENT_DESKTOP=LXQt"
      #     '';
      # };
      desktopEntries = {
        "avahi-discover" = { name = "Avahi Zeroconf Browser"; noDisplay = true; };
        "bssh" = { name = "Avahi SSH Server Browser"; noDisplay = true; };
        "bvnc" = { name = "Avahi VNC Server Browser"; noDisplay = true; };
        "kvantummanager" = { name = "Kvantum Manager"; noDisplay = true; };
        "lxqt-archiver" = { name = "LXQt File Archiver"; noDisplay = true; };
        "nm-tray" = { name = "nm-tray"; noDisplay = true; };
        "picom" = { name = "picom"; noDisplay = true; };
        "qt5ct" = { name = "Qt5 Settings"; noDisplay = true; };
        "qt6ct" = { name = "Qt6 Settings"; noDisplay = true; };
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
