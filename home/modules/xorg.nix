{ config, lib, pkgs, ... }:
let inherit (config.modules.xorg) enable; in {
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
  };
}
