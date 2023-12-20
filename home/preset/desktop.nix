{ config, lib, pkgs, ... }:
{
  imports = [ ../module ];

  config = lib.mkMerge [
    {
      #
      # desktop packages
      #

      home.packages = [
        # cli apps
        pkgs.alsa-utils
        pkgs.pamixer
        pkgs.xclip

        # desktop components
        pkgs.lxqt.lxqt-openssh-askpass # ssh prompter
        pkgs.lxqt.lxqt-policykit # policykit prompter
        pkgs.lxqt.lxqt-sudo # gui-sudo prompter
        pkgs.lxqt.xdg-desktop-portal-lxqt # qt integration with xdg-desktop-portal

        # desktop environment apps
        pkgs.arandr # manual display config
        pkgs.libsForQt5.kdialog # dialogs and widgets
        pkgs.libsForQt5.networkmanager-qt # network manager
        pkgs.lxqt.pavucontrol-qt # sound mixer
        pkgs.lxqt.pcmanfm-qt # file manager

        # desktop apps
        pkgs.lxqt.qps # system monitor
        pkgs.mpc-qt # video player [or haruna/QMPlay2/kmplayer/dragonplayer/mpc-qt/mpv]
        pkgs.copyq # clipboard manager [or: qlipper]
        pkgs.featherpad # simple txt editor
        pkgs.libsForQt5.kolourpaint # simple image editor [or pkgs.photoflare]
        pkgs.libsForQt5.vvave # music player [or elisa/vvave]
        pkgs.qalculate-qt # calculator

        # personal apps
        pkgs.anydesk
        pkgs.gimp
        pkgs.languagetool
        pkgs.libsForQt5.audiotube # not working until https://github.com/NixOS/nixpkgs/issues/228179 and https://github.com/NixOS/nixpkgs/pull/273263 are done
        pkgs.logseq
        pkgs.moonlight-qt
        pkgs.mpv
        pkgs.notepadqq
        pkgs.parsec-bin
        pkgs.qbittorrent # torrenting
        pkgs.soulseekqt
        pkgs.steam
        pkgs.webcord-vencord
        pkgs.ytui-music # using until audiotube is fixed
      ];

      #
      # startx
      #

      xsession.enable = true;

      # last if block from /etc/X11/xinit/xinitrc
      home.file.".xinitrc" = {
        executable = true;

        text =
          ''
            #!/bin/sh

            if [ -d /etc/X11/xinit/xinitrc.d ] ; then
              for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
                [ -x "$f" ] && . "$f"
              done

              unset f
            fi

            [[ -f ~/.xsession ]] && . ~/.xsession
          '';
      };

      #
      # config files
      #

      xdg = {
        enable = true;

        userDirs.enable = true;

        configFile = {
          "xdg-desktop-portal/portals.conf".text =
            ''
              [preferred]
              default=gtk
              org.freedesktop.impl.portal.FileChooser=lxqt
            '';

          "systemd/user/xdg-desktop-portal.service.d/override.conf".text =
            ''
              [Service]
              Environment="XDG_CURRENT_DESKTOP=KDE"
            '';
        };
      };

      services.xsettingsd.enable = true;

      #
      # module configs
      #

      extra = {
        betterlockscreen.enable = true;
        browser.enable = true;
        alacritty.enable = true;
        fontconfig.enable = true;
        fonts.enable = true;
        hotkeys.enable = true;
        loopback-toggle.enable = true;
        menu.enable = true;
        notifications.enable = true;
        polybar.enable = true;
        qview.enable = true;
        screenshot.enable = true;
        themes.enable = true;
        window-manager.enable = true;
      };
    }
    (lib.mkIf config.targets.genericLinux.enable {

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
  ];
}
