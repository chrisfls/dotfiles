{ config, lib, pkgs, ... }:
let inherit (config.modules.xorg) enable; in {
  # TODO: pacman

  options.modules.xorg.enable = lib.mkEnableOption "Enable xorg module";

  config = lib.mkIf enable {
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
      pkgs.xclip
      pkgs.xdotool
      pkgs.xorg.xev
      pkgs.xorg.xkill
      pkgs.xorg.xset
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

    # TODO: move to desktop
    modules.i3wm = {
      startup = [
        "copyq"
        "nm-tray"
        "webcord --start-minimized"
        "whatsapp-for-linux"
        # ferdium
      ];

      apps = {
        "c" = "io.github.Qalculate.qalculate-qt";
        "d" = "webcord";
        "e" = "pcmanfm-qt";
        "shift+c" = "com.github.hluk.copyq";
        "w" = "com.github.eneshecan.WhatsAppForLinux";
        # "f" = "ferdium";
      };
      
      extraConfig = "bindsym Control+Mod1+Delete exec --no-startup-id gtk-launch qps";
    };

    xsession = {
      enable = true;
      numlock.enable = true;
      initExtra =
        ''
          xset s off -dpms
        '';
    };

    services.xsettingsd.enable = true;
  };
}
