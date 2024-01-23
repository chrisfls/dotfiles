{ config, lib, pkgs, ... }:
let
  inherit (config.presets) non-nixos;
  inherit (config.modules.xorg) enable xsession imported-variables window-manager;

  vars = lib.trivial.pipe imported-variables [
    (builtins.map (name: "'${name}'"))
    (builtins.concatStringsSep " ")
  ];

  xrdb =
    if non-nixos then "/usr/bin/xrdb"
    else "${pkgs.xorg.xrdb}/bin/xrdb";

  setxkbmap =
    if non-nixos then "/usr/bin/setxkbmap"
    else "${pkgs.xorg.setxkbmap}/bin/setxkbmap";

  xplugd =
    if non-nixos then "/usr/bin/xplugd"
    else "${pkgs.xplugd}/bin/xplugd";

  xsetroot =
    if non-nixos then "/usr/bin/xsetroot"
    else "${pkgs.xorg.xsetroot}/bin/xsetroot";

  numlockx =
    if non-nixos then "/usr/bin/numlockx"
    else "${pkgs.numlockx}/bin/numlockx";

  cursorPath = "${config.modules.themes.cursor.package}/share/icons/${lib.strings.escapeShellArg config.home.pointerCursor.name}/cursors/${
    lib.strings.escapeShellArg config.home.pointerCursor.x11.defaultCursor
  }";
in
{
  options.modules.xorg = {
    enable = lib.mkEnableOption "Enable xorg module";
    xsession = lib.mkOption { type = lib.types.lines; default = ""; };
    window-manager = lib.mkOption { type = lib.types.str; default = ""; };
    imported-variables = lib.mkOption { type = lib.types.listOf lib.types.str; default = [ ]; };
    cursor = {
      name = "";
      size = { type = lib.types.int; default = 36; };
    };
  };

  config = lib.mkIf enable {
    pacman = {
      packages = [
        "extra/xorg-xinit"
        "extra/xorg-xinput"
        "extra/xorg-xsetroot"
        "extra/numlockx"
        "aur/xplugd"
      ];
      pkgs.xorg = {
        xmodmap = [ "extra/xorg-xmodmap" ];
        xrdb = [ "extra/xorg-xrdb" ];
      };
    };

    home.packages = lib.mkIf (!non-nixos) [
      pkgs.numlockx
      pkgs.xclip
      pkgs.xdotool
      pkgs.xorg.numlockx
      pkgs.xorg.setxkbmap
      pkgs.xorg.xev
      pkgs.xorg.xkill
      pkgs.xorg.xrdb
      pkgs.xorg.xset
      pkgs.xorg.xsetroot
      pkgs.xplugd
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
        "whatsapp-for-linux"
        (if non-nixos then "vesktop --start-minimized" else "webcord --start-minimized")
      ];

      apps = {
        "c" = "io.github.Qalculate.qalculate-qt";
        "d" = if non-nixos then "vesktop" else "webcord";
        "e" = "pcmanfm-qt";
        "shift+c" = "com.github.hluk.copyq";
        "w" = "com.github.eneshecan.WhatsAppForLinux";
      };

      extraConfig = "bindsym Control+Mod1+Delete exec --no-startup-id gtk-launch qps";
    };

    home.file = {
      ".xsession".text =
        ''
          if [ -z "$HM_XPROFILE_SOURCED" ]; then
            . "${config.home.homeDirectory}/.xprofile"
          fi
          unset HM_XPROFILE_SOURCED

          systemctl --user start hm-graphical-session.target

          xset s off -dpms

          ${xsession}
          ${xrdb} -merge ${config.home.homeDirectory}/.Xresources
        
          ${xsetroot} -xcf ${cursorPath} ${toString config.modules.themes.cursor.size}

          ${window-manager}

          systemctl --user stop graphical-session.target
          systemctl --user stop graphical-session-pre.target

          # Wait until the units actually stop.
          while [ -n "$(systemctl --user --no-legend --state=deactivating list-units)" ]; do
            sleep 0.5
          done

          systemctl --user unset-environment ${vars}
        '';

      ".xprofile".text =
        ''
          . "${config.home.homeDirectory}/.nix-profile/etc/profile.d/hm-session-vars.sh"

          if [ -e "$HOME/.profile" ]; then
            . "$HOME/.profile"
          fi

          # If there are any running services from a previous session.
          # Need to run this in xprofile because the NixOS xsession
          # script starts up graphical-session.target.
          systemctl --user stop graphical-session.target graphical-session-pre.target

          systemctl --user import-environment ${vars}

          export HM_XPROFILE_SOURCED=1
        '';
    };

    # TODO: hardcode
    services.xsettingsd.enable = true;

    systemd.user = {
      services = lib.mkIf (config.home.keyboard != null) {
        setxkbmap = {
          Unit = {
            Description = "Set up keyboard in X";
            After = [ "graphical-session-pre.target" ];
            PartOf = [ "graphical-session.target" ];
          };

          Install = { WantedBy = [ "graphical-session.target" ]; };

          Service = {
            Type = "oneshot";
            RemainAfterExit = true;
            ExecStart = with config.home.keyboard;
              let
                args = lib.optional (layout != null) "-layout '${layout}'"
                  ++ lib.optional (variant != null) "-variant '${variant}'"
                  ++ lib.optional (model != null) "-model '${model}'"
                  ++ [ "-option ''" ] ++ map (v: "-option '${v}'") options;
              in
              "${setxkbmap} ${toString args}";
          };
        };

        xplugd = {
          Unit = {
            Description = "Rerun setxkbmap.service when I/O is changed";
            After = [ "graphical-session-pre.target" ];
            PartOf = [ "graphical-session.target" ];
          };

          Install = { WantedBy = [ "graphical-session.target" ]; };

          Service = {
            Type = "forking";
            Restart = "on-failure";
            ExecStart =
              let
                script = pkgs.writeShellScript "xplugrc" ''
                  case "$1,$3" in
                    keyboard,connected)
                    systemctl --user restart setxkbmap.service
                    ;;
                  esac
                '';
              in
              "${xplugd} ${script}";
          };
        };

        numlockx = {
          Unit = {
            Description = "NumLockX";
            After = [ "graphical-session-pre.target" ];
            PartOf = [ "graphical-session.target" ];
          };

          Service = {
            Type = "oneshot";
            RemainAfterExit = true;
            ExecStart = numlockx;
          };

          Install = { WantedBy = [ "graphical-session.target" ]; };
        };
      };

      targets = {
        hm-graphical-session = {
          Unit = {
            Description = "Home Manager X session";
            Requires = [ "graphical-session-pre.target" ];
            BindsTo = [ "graphical-session.target" "tray.target" ];
          };
        };

        tray = {
          Unit = {
            Description = "Home Manager System Tray";
            Requires = [ "graphical-session-pre.target" ];
          };
        };
      };
    };
  };
}
