{ config, lib, pkgs, specialArgs, ... }:
let
  inherit (specialArgs) qt;
  inherit (config.preset) development non-nixos;

  enable = config.preset.desktop;


  # needed until these are done:
  #  - https://github.com/NixOS/nixpkgs/issues/228179
  #  - https://github.com/NixOS/nixpkgs/pull/273263 
  #audiotube = pkgs.libsForQt5.audiotube.overrideAttrs (old: {
  #  buildInputs = old.buildInputs ++ [ pkgs.libsForQt5.kpurpose ];
  #});

  /*hahaha =
    (pkgs.stdenvNoCC.mkDerivation {
      pname = pkgs.i3.pname;
      version = pkgs.i3.version;
      src = pkgs.i3;
      dontBuild = false;
      /*installPhase = ''
          find $src -type f | while read -r line; do
            file="$(realpath --relative-to="$src" "$line")"

            mkdir -p $(dirname "$out/$fil"
          donee")
            ln -s "$src/$file" "$out/$file
        '';
    });
        */

in

{
  options.preset.desktop = lib.mkEnableOption "Enable desktop preset";

  config = lib.mkIf enable (lib.mkMerge [
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
        xdg-desktop-portal.enable = true;
        # sxhkd = {
        #   enable = false;
        #   keybindings = {
        #     "ctrl + alt + Delete" = "gtk-launch qps";
        #     "super + a ; c" = "gtk-launch com.github.hluk.copyq.desktop";
        #     "super + a ; e" = "gtk-launch pcmanfm-qt";
        #   };
        # };
      };

      xsession = {
        enable = true;
        numlock.enable = true;
        windowManager.i3.config = {
          keybindings."ctrl+alt+Delete" = "exec gtk-launch qps";

          modes.apps = {
            "c" = "exec gtk-launch com.github.hluk.copyq";
            "d" = "exec gtk-launch webcord";
            "e" = "exec gtk-launch pcmanfm-qt";
            "w" = "exec gtk-launch com.github.eneshecan.WhatsAppForLinux";
          };

          startup = [
            { notification = false; command = "${pkgs.usr.copyq}/bin/copyq"; }
            { notification = false; command = "${pkgs.usr.nm-tray}/bin/nm-tray"; }
            { notification = false; command = "${pkgs.webcord-vencord}/bin/webcord --start-minimized"; }
            { notification = false; command = "${pkgs.usr.whatsapp-for-linux}/bin/whatsapp-for-linux --start-minimized"; }
          ];
        };
      };

      # xsession.windowManager.bspwm.startupPrograms = [
      #   "${pkgs.copyq}/bin/copyq"
      #   "${pkgs.nm-tray}/bin/nm-tray"
      #   "env -u QT_SCREEN_SCALE_FACTORS ${pkgs.telegram-desktop}/bin/telegram-desktop -startintray"
      #   "${pkgs.webcord-vencord}/bin/webcord --start-minimized"
      #   "${pkgs.whatsapp-for-linux}/bin/whatsapp-for-linux --start-minimized"
      #   "${pkgs.jamesdsp}/bin/bin/jamesdsp"
      # ];


      nixpkgs.config.permittedInsecurePackages = [ "electron-25.9.0" ];

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

      home.packages = [
        # cli apps
        pkgs.alsa-utils
        pkgs.pamixer
        pkgs.xclip
        pkgs.xorg.xev
        pkgs.xtitle # TODO: check if needed
        pkgs.xorg.xkill
        pkgs.xdotool

        # desktop components
        pkgs.usr.lxqt.lxqt-openssh-askpass # ssh prompter
        pkgs.usr.lxqt.lxqt-policykit # policykit prompter
        pkgs.usr.lxqt.lxqt-sudo # gui-sudo prompter

        # desktop environment apps
        pkgs.usr.libsForQt5.kdialog # dialogs and widgets
        pkgs.usr.nm-tray # network manager
        pkgs.usr.lxqt.pavucontrol-qt # sound mixer
        pkgs.usr.lxqt.pcmanfm-qt # file manager
        pkgs.usr.lxqt.lxqt-archiver # archiver

        # desktop apps
        pkgs.usr.lxqt.qps # system monitor
        pkgs.usr.mpc-qt # video player [or haruna/QMPlay2/kmplayer/dragonplayer/mpv]
        pkgs.usr.copyq # clipboard manager [or: qlipper]
        pkgs.usr.featherpad # simple txt editor
        pkgs.usr.libsForQt5.kolourpaint # simple image editor [or pkgs.photoflare]
        pkgs.usr.libsForQt5.vvave # music player [or elisa/vvave]
        pkgs.usr.qalculate-qt # calculator

        # personal apps
        # pkgs.logseq
        pkgs.usr.anydesk
        pkgs.usr.gimp
        pkgs.usr.moonlight-qt
        pkgs.usr.parsec-bin
        pkgs.usr.qbittorrent
        pkgs.usr.rclone
        # pkgs.soulseekqt
        # pkgs.steam
        pkgs.webcord-vencord
        pkgs.usr.whatsapp-for-linux
        pkgs.usr.libsForQt5.audiotube
      ];

      pacman.usr = {
        anydesk = [ "chaotic-aur/anydesk-bin" ];
        copyq = [ "extra/copyq" ];
        featherpad = [ "extra/featherpad" ];
        gimp = [ "extra/gimp" ];
        libsForQt5.audiotube = [ "extra/audiotube" ];
        libsForQt5.kdialog = [ "extra/kdialog" ];
        libsForQt5.kolourpaint = [ "extra/kolourpaint" ];
        libsForQt5.vvave = [ "extra/vvave" ];
        lxqt.lxqt-archiver = [ "extra/lxqt-archiver" ];
        lxqt.lxqt-openssh-askpass = [ "extra/lxqt-openssh-askpass" ];
        lxqt.lxqt-policykit = [ "extra/lxqt-policykit" ];
        lxqt.lxqt-sudo = [ "extra/lxqt-sudo" ];
        lxqt.pavucontrol-qt = [ "extra/pavucontrol-qt" ];
        lxqt.pcmanfm-qt = [ "extra/pcmanfm-qt" ];
        lxqt.qps = [ "extra/qps" ];
        moonlight-qt = [ "chaotic-aur/moonlight-qt" ];
        mpc-qt = [ "chaotic-aur/mpc-qt" ];
        nm-tray = [ "chaotic-aur/nm-tray" ];
        parsec-bin = [ "chaotic-aur/parsec-bin" ];
        qalculate-qt = [ "extra/qalculate-qt" ];
        qbittorrent = [ "extra/qbittorrent" ];
        rclone = [ "extra/rclone" ];
        whatsapp-for-linux = [ "chaotic-aur/whatsapp-for-linux" ];
        xclip = [ "extra/xclip" ];
      };

      # services.xsettingsd.package


      pacman.nix = {
        alsa-utils = [ "extra/alsa-utils" ];
        pamixer = [ "extra/pamixer" ];
        xdotool = [ "extra/xdotool" ];
        xorg.xev = [ "extra/xorg-xev" ];
        xorg.xinit = [ "extra/xorg-xinit" ];
        xorg.xinput = [ "extra/xorg-xinput" ];
        xorg.xkill = [ "extra/xorg-xkill" ];
        xorg.xrdb = [ "extra/xorg-xrdb" ];
        xorg.xsetroot = [ "extra/xorg-xsetroot" ];
        xsettingsd = [ "extra/xsettingsd" ];
      };

      xdg = {
        enable = true;
        mimeApps.enable = true;
        userDirs.enable = true;
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
          "rofi-theme-selector" = { name = ""; noDisplay = true; terminal = true; };
          "rofi" = { name = ""; noDisplay = true; terminal = true; };
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

      services.xsettingsd.enable = true;
      services.udiskie.enable = true;
    }
    (lib.mkIf development {
      module = {
        code.enable = true;
        sublime.enable = true;
        #helix.enable = true;
        #emacs.enable = true;
      };
    })
    (lib.mkIf non-nixos {
      xsession.importedVariables = [
        "PATH"
        "LIBGL_DRIVERS_PATH"
        "LIBVA_DRIVERS_PATH"
        "__EGL_VENDOR_LIBRARY_FILENAMES"
        "LD_LIBRARY_PATH"
        "VK_ICD_FILENAMES"
      ];

      home.sessionVariables = {
        LIBGL_DRIVERS_PATH = "/usr/lib/dri";
        LIBVA_DRIVERS_PATH = "/usr/lib/dri";
        __EGL_VENDOR_LIBRARY_FILENAMES = "/usr/share/glvnd/egl_vendor.d/50_mesa.json";
        # vulkan  will not work without 
        # zlib-*/lib
        # libdrm-*/lib
        # libX11-*/lib
        # libxcb-*/lib
        # libxshmfence-*/lib
        # wayland-*/lib
        # gcc-*/lib
        LD_LIBRARY_PATH = "/usr/lib:/usr/lib/vdpau:$LD_LIBRARY_PATH";
        VK_ICD_FILENAMES = "$(find /usr/share/vulkan/icd.d/ -name '*.json' | tr '\\n' ':' | sed 's/:$//')";
      };
    })
  ]);
}
