{ config, lib, pkgs, specialArgs, ... }:
let
  inherit (config.preset) desktop development non-nixos;

  mesa = specialArgs.mesa.wrapIf non-nixos;

  mesa-packages = map (pkg: mesa { pkg = pkg; });

  # needed until these are done:
  #  - https://github.com/NixOS/nixpkgs/issues/228179
  #  - https://github.com/NixOS/nixpkgs/pull/273263 
  #audiotube = pkgs.libsForQt5.audiotube.overrideAttrs (old: {
  #  buildInputs = old.buildInputs ++ [ pkgs.libsForQt5.kpurpose ];
  #});
in

{
  options.preset.desktop = lib.mkEnableOption "Enable desktop preset";

  config = lib.mkIf desktop (lib.mkMerge [
    {
      module = {
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
        wezterm.enable = true;
        # xdg-desktop-portal.enable = true;
      };

      pacman =
        {
          packages = [
            "extra/xorg-xinit"
            "extra/xorg-xinput"
          ];
          pkgs = {
            nm-tray = [ "chaotic-aur/nm-tray" ];
            xorg = {
              xmodmap = [ "extra/xorg-xmodmap" ];
              xrdb = [ "extra/xorg-xrdb" ];
            };
            lxqt.qps = [ "extra/qps" ];
            libsForQt5.audiotube = [ "extra/audiotube" ];
          };
        };

      nixpkgs.config.permittedInsecurePackages = [ "electron-25.9.0" ];

      home.packages = [
        # cli pkgs
        pkgs.alsa-utils
        pkgs.pamixer
        pkgs.xclip
        pkgs.xdotool
        pkgs.xorg.xev
        pkgs.xorg.xkill

        # desktop pkgs
        (mesa { pkg = pkgs.libsForQt5.kdialog; exe = "kdialog"; }) # dialogs and widgets
        (mesa { pkg = pkgs.lxqt.lxqt-openssh-askpass; exe = "lxqt-openssh-askpass"; }) # ssh prompter
        (mesa { pkg = pkgs.lxqt.lxqt-policykit; exe = "lxqt-policykit-agent"; }) # policykit prompter
        (mesa { pkg = pkgs.lxqt.lxqt-sudo; exe = "lxqt-sudo"; }) # gui-sudo prompter
        pkgs.nm-tray # network manager tray icon

        # desktop apps
        (mesa { pkg = pkgs.lxqt.lxqt-archiver; exe = "lxqt-archiver"; }) # archiver
        (mesa { pkg = pkgs.lxqt.pavucontrol-qt; exe = "pavucontrol-qt"; }) # sound mixer
        (mesa { pkg = pkgs.lxqt.pcmanfm-qt; exe = "pcmanfm-qt"; }) # file manager

        # common apps
        # (mesa { pkg = pkgs.copyq; exe = "copyq"; }) # clipboard manager [ TODO: qt6 ]
        (mesa { pkg = pkgs.featherpad; exe = "featherpad"; }) # simple text editor
        (mesa { pkg = pkgs.libsForQt5.kcalc; exe = "kcalc"; }) # calculator
        (mesa { pkg = pkgs.libsForQt5.kolourpaint; exe = "kolourpaint"; }) # simple image editor
        (mesa { pkg = pkgs.libsForQt5.vvave; exe = "vvave"; }) # music player [or elisa]
        (mesa { pkg = pkgs.mpc-qt; exe = "mpc-qt"; }) # video player [or haruna/QMPlay2/kmplayer/dragonplayer/mpv]
        pkgs.lxqt.qps # system monitor

        # personal cli pkgs
        pkgs.rclone

        # personal apps
        (mesa { pkg = pkgs.anydesk; exe = "anydesk"; })
        (mesa { pkg = pkgs.gimp; exe = "gimp"; })
        # (mesa { pkg = pkgs.logseq; exe = "logseq"; })
        (mesa { pkg = pkgs.moonlight-qt; exe = "moonlight"; })
        (mesa { pkg = pkgs.parsec-bin; exe = "parsecd"; })
        (mesa { pkg = pkgs.pkgs.webcord-vencord; })
        # (mesa { pkg = pkgs.qbittorrent-qt5; exe = "qbittorrent"; }) # broken tray icon
        # (mesa { pkg = pkgs.soulseekqt; }) # broken tray icon
        (mesa { pkg = pkgs.whatsapp-for-linux; exe = "whatsapp-for-linux"; })
        pkgs.libsForQt5.audiotube
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
        mimeApps.enable = true;
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
          keybindings."Control+Mod1+Delete" = "exec gtk-launch qps";

          modes.apps = {
            # "c" = "exec gtk-launch copyq; mode default";
            "d" = "exec gtk-launch webcord; mode default";
            "e" = "exec gtk-launch pcmanfm-qt; mode default";
            "w" = "exec gtk-launch com.github.eneshecan.WhatsAppForLinux; mode default";
          };

          startup = [
            # { notification = false; command = "copyq"; }
            { notification = false; command = "nm-tray"; }
            { notification = false; command = "webcord --start-minimized"; }
            { notification = false; command = "whatsapp-for-linux"; }
          ];
        };
      };

      services.xsettingsd.enable = true;
      services.udiskie.enable = true;
    }
    (lib.mkIf development {
      module = {
        code.enable = true;
        sublime.enable = true;
        # helix.enable = true;
        # emacs.enable = true;
      };
    })
  ]);
}
