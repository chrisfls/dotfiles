{ config, lib, pkgs, specialArgs, ... }:
let inherit (config.presets) desktop; in {
  options.presets.desktop = lib.mkEnableOption "Enable desktop preset";

  config = lib.mkIf desktop {
    modules = {
      autorandr.enable = true;
      brave.enable = true;
      dunst.enable = true;
      fontconfig.enable = true;
      fonts.enable = true;
      i3wm.enable = true;
      jamesdsp.enable = true;
      loopback-toggle.enable = true;
      micro = { enable = true; desktop = true; };
      picom.enable = true;
      polybar.enable = true;
      qview.enable = true;
      rofi.enable = true;
      screenshot.enable = true;
      telegram.enable = true;
      themes.enable = true;
      kitty.enable = true;
      xdg-desktop-portal.enable = true;
    };

    pacman = {
      packages = [
        "extra/xorg-xinit"
        "extra/xorg-xinput"
      ];
      pkgs.xorg = {
        xmodmap = [ "extra/xorg-xmodmap" ];
        xrdb = [ "extra/xorg-xrdb" ];
      };
    };

    home.packages = [
      # cli pkgs
      pkgs.alsa-utils
      pkgs.pamixer
      pkgs.xclip
      pkgs.xdotool
      pkgs.xorg.xev
      pkgs.xorg.xkill
      pkgs.xdg-utils

      # desktop pkgs
      pkgs.libsForQt5.kdialog # dialogs and widgets
      pkgs.lxqt.lxqt-openssh-askpass # ssh prompter
      pkgs.lxqt.lxqt-policykit # policykit prompter
      pkgs.lxqt.lxqt-sudo # gui-sudo prompter
      pkgs.nm-tray # network manager

      # desktop apps
      pkgs.lxqt.lxqt-archiver # archiver
      pkgs.lxqt.pavucontrol-qt # sound mixer
      pkgs.lxqt.pcmanfm-qt # file manager

      # common apps
      pkgs.copyq # clipboard manager
      pkgs.featherpad # simple text editor
      pkgs.qalculate-qt # calculator
      pkgs.libsForQt5.kolourpaint # simple image editor
      pkgs.libsForQt5.juk # music player [or elisa]
      pkgs.lxqt.qps # system monitor
      pkgs.mpc-qt # video player [or haruna/QMPlay2/kmplayer/dragonplayer/mpv]

      # personal cli pkgs
      pkgs.rclone

      # personal apps
      pkgs.qbittorrent
      pkgs.anydesk
      pkgs.gimp
      pkgs.moonlight-qt
      pkgs.parsec-bin
      pkgs.webcord-vencord
      pkgs.whatsapp-for-linux
      pkgs.logseq
      # pkgs.soulseekqt;

      # needed until these are done:
      #  - https://github.com/NixOS/nixpkgs/issues/228179
      #  - https://github.com/NixOS/nixpkgs/pull/273263 
      (pkgs.libsForQt5.audiotube.overrideAttrs (old: {
        buildInputs = old.buildInputs ++ [ pkgs.libsForQt5.kpurpose ];
      }))
    ];

    home.file.".xinitrc" = {
      executable = true;
      text =
        ''
          #!/bin/sh

          # this is the last if block from /etc/X11/xinit/xinitrc
          if [ -d /etc/X11/xinit/xinitrc.d ] ; then
            for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
              [ -x "$f" ] && . "$f"
            done

            unset f
          fi

          [[ -f ~/.xsession ]] && . ~/.xsession
        '';
    };

    xdg = {
      enable = true;
      mimeApps = {
        enable = true;
        defaultApplications = {
          "inode/directory" = "pcmanfm-qt.desktop";
          "text/plain" = "featherpad.desktop";
          "application/zip" = "lxqt-archiver.desktop";
          "application/rar" = "lxqt-archiver.desktop";
          "application/7z" = "lxqt-archiver.desktop";
          "application/*tar" = "lxqt-archiver.desktop";
          "video/*" = "io.github.mpc_qt.Mpc-Qt.desktop";
          "audio/*" = "org.kde.vvave.desktop";
          "x-scheme-handler/tg" = "org.telegram.desktop.desktop";
        };
      };
      userDirs.enable = true;
      configFile."kwalletrc".text =
        ''
          [Wallet]
          Enabled=false
        '';
      desktopEntries = {
        "avahi-discover" = { name = "Avahi Zeroconf Browser"; noDisplay = true; };
        "bssh" = { name = "Avahi SSH Server Browser"; noDisplay = true; };
        "bvnc" = { name = "Avahi VNC Server Browser"; noDisplay = true; };
        "kvantummanager" = { name = "Kvantum Manager"; noDisplay = true; };
        "lxqt-archiver" = { name = "LXQt File Archiver"; noDisplay = true; };
        "nm-tray" = { name = "nm-tray"; noDisplay = true; };
        "pcmanfm-qt-desktop-pref" = { name = ""; noDisplay = true; };
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

    xsession = {
      enable = true;
      numlock.enable = true;
      windowManager.i3.config = {
        keybindings."Control+Mod1+Delete" = "exec --no-startup-id gtk-launch qps";

        modes.apps = {
          "c" = "exec --no-startup-id gtk-launch io.github.Qalculate.qalculate-qt; mode default";
          "shift+c" = "exec --no-startup-id gtk-launch com.github.hluk.copyq; mode default";
          "d" = "exec --no-startup-id  gtk-launch webcord; mode default";
          "e" = "exec --no-startup-id  gtk-launch pcmanfm-qt; mode default";
          "w" = "exec --no-startup-id  gtk-launch com.github.eneshecan.WhatsAppForLinux; mode default";
        };

        startup = [
          { notification = false; command = "copyq"; }
          { notification = false; command = "nm-tray"; }
          { notification = false; command = "webcord --start-minimized"; }
          { notification = false; command = "whatsapp-for-linux"; }
        ];
      };
    };

    services.xsettingsd.enable = true;
    services.udiskie.enable = true;
  };
}
