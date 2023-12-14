{ config, lib, pkgs, specialArgs, ... }:
let
  color-scheme = specialArgs.color-schemes.popping-and-locking-black;
  qt-theme = pkgs.libsForQt5.breeze-qt5;
  gtk-theme = pkgs.libsForQt5.breeze-gtk;
  icon-theme = pkgs.libsForQt5.breeze-icons;

  breeze = "Breeze";
  breeze-lo = "breeze";
  breezeDark = "BreezeDark";
  breeze--Dark = "Breeze Dark";
  lookAndFeelPackage = "org.kde.breezedark.desktop";
  breeze-dark = "Breeze-Dark";
  breeze-cursors = "breeze_cursors";
in
{
  gtk = {
    enable = true;
    theme = {
      name = breeze-dark;
      package = gtk-theme;
    };
    iconTheme = {
      name = breeze;
      package = icon-theme;
    };
  };

  qt = {
    enable = true;
    platformTheme = "kde";
    style = {
      name = breeze-lo;
      package = qt-theme;
    };
  };

  home.pointerCursor = {
    name = breeze-cursors;
    package = qt-theme;
    size = 24;
    gtk.enable = true;
    x11 = {
      enable = true;
      defaultCursor = breeze-cursors;
    };
  };

  xdg = {
    enable = true;
    userDirs = {
      enable = true;
      createDirectories = true;
    };
    configFile = {
      kdeglobals.text = lib.generators.toINI { } {
        General = {
          ColorScheme = breezeDark;
          Name = breeze--Dark;
          shadeSortColumn = true;
        };
        KDE = {
          LookAndFeelPackage = lookAndFeelPackage;
          contrast = 4;
          widgetStyle = breeze;
        };
        Icons.Theme = breeze;
      };
      kcminputrc.text = lib.generators.toINI { } {
        Mouse = {
          cursorTheme = breeze-cursors;
          cursorSize = 24;
        };
      };
    };
  };

}

