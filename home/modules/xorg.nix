{ config, lib, pkgs, ... }:
let
  inherit (config.modules.xorg) enable xsession imported-variables window-manager;

  vars = lib.trivial.pipe imported-variables [
    (builtins.map (name: "'${name}'"))
    (builtins.concatStringsSep " ")
  ];

  xrdb = "/usr/bin/xrdb";
  setxkbmap = "/usr/bin/setxkbmap";
  xplugd = "/usr/bin/xplugd";
  xsetroot = "/usr/bin/xsetroot";
  numlockx = "/usr/bin/numlockx";

  cursorPath = "/usr/share/icons/${lib.strings.escapeShellArg config.modules.themes.cursor.name}/cursors/left_ptr";
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
    pacman.packages = [
      "extra/xclip"
      "extra/xdotool"
      "extra/numlockx"
      "extra/xorg-setxkbmap"
      "extra/xorg-xev"
      "extra/xorg-xkill"
      "extra/xorg-xrdb"
      "extra/xorg-xset"
      "extra/xorg-xsetroot"
      "aur/xplugd"
      "extra/xorg-xinit"
      "extra/xorg-xinput"
      "extra/xorg-xmodmap"
      "extra/xsettingsd"
    ];

    home.file = {
      ".xinitrc" = {
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

      ".xsession".text =
        ''
          if [ -z "$HM_XPROFILE_SOURCED" ]; then
            . "$HOME/.xprofile"
          fi
          unset HM_XPROFILE_SOURCED

          systemctl --user start hm-graphical-session.target

          xset s off -dpms

          ${xsession}
          ${xrdb} -merge $HOME/.Xresources
        
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
          . "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh"

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

        xsettingsd = {
          Unit = {
            Description = "xsettingsd";
            After = [ "graphical-session-pre.target" ];
            PartOf = [ "graphical-session.target" ];
          };

          Install.WantedBy = [ "graphical-session.target" ];

          Service = {
            Environment = "PATH=${config.home.profileDirectory}/bin";
            ExecStart = "/usr/bin/xsettingsd";
            Restart = "on-abort";
          };
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
