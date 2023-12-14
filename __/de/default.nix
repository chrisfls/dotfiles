{ config, lib, pkgs, specialArgs, ... }:
with specialArgs;
let
  qt-theme = pkgs.libsForQt5.breeze-qt5;
  gtk-theme = pkgs.libsForQt5.breeze-gtk;
  icon-theme = pkgs.libsForQt5.breeze-icons;
in
{
  imports = [
    ./themes.nix
    ./waybar
    ./hypr
    ./kitty
  ];

  config = {
    theme = config.themes.popping-and-locking;

    fonts.fontconfig.enable = true;

    services.easyeffects.enable = true;

    # TODO:
    # bluedevil
    # brave
    # cameractrls
    # qt6-wayland
    # udiskie
    # udiskie
    home.packages = with pkgs; [

      # tooling
      alsa-utils
      pamixer # change volume from waybar

      # shell
      xdg-desktop-portal-hyprland # screen share
      rofi-wayland # menu
      dunst # notifications
      xdg-launch # launch desktop apps

      # desktop support
      grimblast # print screen
      networkmanagerapplet # network panel
      lxqt.pavucontrol-qt # audio control
      lxqt.pcmanfm-qt # file manager
      btop # system monitor
      libsForQt5.networkmanager-qt

      brave

      # theme
      qt-theme
      gtk-theme
      icon-theme

      # applications
      logseq
      libsForQt5.audiotube

      languagetool
      webcord-vencord
      lxqt.lxqt-sudo
      lxqt.lxqt-sudo
      lxqt.lxqt-notificationd
      lxqt.lxqt-archiver
      lxqt.lximage-qt
      lxqt.screengrab
      lxqt.lxqt-powermanagement
      lxqt.lxqt-policykit
      lxqt.lxqt-openssh-askpass
    ];

    gtk = {
      enable = true;
      theme = {
        name = "Breeze-Dark";
        package = gtk-theme;
      };
      iconTheme = {
        name = "Breeze Dark";
        package = icon-theme;
      };
    };

    home.pointerCursor = {
      name = "breeze_cursors";
      package = qt-theme;
      size = 24;
      gtk.enable = true;
    };

    programs.bash = {
      sessionVariables = {
        QT_QPA_PLATFORMTHEME = "kde";
        # XDG_CONFIG_HOME = "$HOME/.config";
        # XDG_DATA_HOME = "$HOME/.local/share";
        # XDG_STATE_HOME = "$HOME/.local/state";
        # XDG_CACHE_HOME = "$HOME/.cache";
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
            ColorScheme = "BreezeDark";
            Name = "Breeze Dark";
            shadeSortColumn = true;
          };
          KDE = {
            LookAndFeelPackage = "org.kde.breezedark.desktop";
            contrast = 4;
            widgetStyle = "Breeze";
          };
          Icons.Theme = "Breeze";
        };
        kcminputrc.text = lib.generators.toINI { } {
          Mouse = {
            cursorTheme = "breeze_cursors";
            cursorSize = 24;
          };
        };
      };
    };

    # TODO: configure
    services.dunst = {
      # TODO: avizo or dunstify as volume/brightness level indicator or 
      enable = true;
      iconTheme = {
        name = "Adwaita";
        package = pkgs.gnome.adwaita-icon-theme;
        size = "32x32";
      };
      settings = {
        global = {
          width = 300;
          height = 300;
          offset = "30x50";
          origin = "top-right";
          transparency = 10;
          frame_color = "#eceff1";
          font = "Droid Sans 9";
        };

        urgency_normal = {
          background = "#37474f";
          foreground = "#eceff1";
          timeout = 10;
        };
      };
    };
  };
}
