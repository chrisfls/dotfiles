{ config, lib, pkgs, specialArgs, ... }:
let
  inherit (config.module.polybar) enable;
  inherit (config.module.themes) color-scheme;

  polybar-msg = "${config.services.polybar.package}/bin/polybar-msg";
  bluetoothctl = "${pkgs.bluez}/bin/bluetoothctl";
  pamixer = "${pkgs.pamixer}/bin/pamixer";
  dunstctl = "${pkgs.dunst}/bin/dunstctl";

  toggle =
    let
      script = pkgs.writeShellScriptBin "polybar-toggle"
        ''
          fst=$1
          shift

          for arg in "$@"; do
            ${polybar-msg} action "#$arg.module_toggle"
          done

          ${polybar-msg} action $fst next
        '';
    in
    "${script}/bin/polybar-toggle";

  bluetooth =
    let
      script = pkgs.writeShellScriptBin "polybar-bluetooth"
        ''
          if bluetoothctl show | grep -q "Powered: yes"; then
              hook=2
          else
              hook=1
          fi

          if [ "$1" = "--toggle" ]; then
              if [ "$hook" = "2" ]; then
                  bluetoothctl power off
                  hook=1
              else
                  bluetoothctl power on
                  hook=2
              fi
          fi

          ${polybar-msg} action bluetooth hook $hook
        '';
    in
    "${script}/bin/polybar-bluetooth";

  pipewire =
    let
      script = pkgs.writeShellScriptBin "polybar-pipewire"
        ''
          case $1 in
            "--up")
              ${pamixer} --increase 5
              ;;
            "--down")
              ${pamixer} --decrease 5  
              ;;
            "--mute")
              ${pamixer} --toggle-mute
              ;;
          esac

          if [ "$(${pamixer} --get-volume-human)" = "muted" ]; then
            hook=1
          else
            hook=2
          fi

          ${polybar-msg} action audio hook $hook
        '';
    in
    "${script}/bin/polybar-pipewire";

  dunst-toggle =
    let
      script = pkgs.writeShellScriptBin "dunst-toggle"
        ''
          ${dunstctl} set-paused toggle
          ${polybar-msg} action notifications next
        '';
    in
    "${script}/bin/dunst-toggle";

  darkest = color-scheme.background;
  light = color-scheme.foreground;
  dark = color-scheme.black;
  danger = color-scheme.red;
in
{
  options.module.polybar.enable = lib.mkEnableOption "Enable polybar module";

  config = lib.mkIf enable {
    # xsession.windowManager.bspwm.startupPrograms = [
    #   "systemd-cat -t polybar systemd-run --user --scope --property=OOMPolicy=continue -u polybar ${pkgs.polybar}/bin/polybar"
    # ];

    # xsession.windowManager.i3.config.startup = [
    #   {
    #     command = "${pkgs.polybar}/bin/polybar topbar -r";
    #     notification = false;
    #   }
    # ];

    # pacman.usr.polybarFull = [ "chaotic-aur/polybar-git" ];

    systemd.user.services.polybar = {
      # i3wm fix
      Unit.After = lib.mkIf (config.module.i3wm.enable) [ "graphical-session-i3.target" ];
      Install.WantedBy = lib.mkIf (config.module.i3wm.enable) (lib.mkForce [ "graphical-session-i3.target" ]);

      # home manager fix
      Service.Environment = lib.mkIf (config.preset.non-nixos) (lib.mkForce [ ]);
    };

    # i3wm launch
    xsession.windowManager.i3.config.startup = lib.mkIf (config.module.i3wm.enable) [
      {
        command = "systemctl --user restart polybar.service";
        always = true;
        notification = false;
      }
    ];

    services.polybar = {
      enable = true;
      package = pkgs.polybarFull;
      script = "polybar topbar &";
      settings = {
        settings.screenchange-reload = "\"true\"";
        "bar/topbar" = {
          # settings
          enable-ipc = "\"true\"";

          # layout
          dpi = "\"144\"";
          height = "\"18pt\"";
          width = "\"100%\"";
          line-size = "\"2pt\"";
          padding-left = "\"0\"";
          padding-right = "\"0\"";

          # styling

          # base font
          font-0 = "\"CaskaydiaCoveNerdFontMono:size=10;4\"";

          # icon font
          font-1 = "\"CaskaydiaCoveNerdFontPropo:size=11;3\"";

          # powerline font
          font-2 = "\"CaskaydiaCoveNerdFont:size=20;6\"";

          background = "\"${darkest}\"";
          foreground = "\"${light}\"";

          # modules
          modules-left = "\"menu menu-l workspaces even-l toggle notifications tray right\"";
          modules-center = "\"left title right \"";
          modules-right = "\"left filesystem even-r temperature odd-r memory even-r cpu odd-r wired even-r wireless odd-r bluetooth even-r audio odd-r date even-r time menu-r session\"";
          format-radius = "\"32.0\"";
        };
        "module/menu" = {
          type = "\"custom/ipc\"";

          click-left = "\"rofi-menu\"";

          # startup
          initial = "\"1\"";
          # move
          hook-0 = "\"\"";
          format-0 = "\" %{T2}󰺮%{T-} Applications\"";
          format-0-background = "\"${darkest}\"";
          format-0-foreground = "\"${light}\"";
          format-0-padding = "\"0\"";
          # resize
          hook-1 = "\"\"";
          format-1 = "\" %{T2}󰺮%{T-} Applications\"";
          format-1-background = "\"${darkest}\"";
          format-1-foreground = "\"${danger}\"";
          format-1-padding = "\"0\"";
        };
        "module/session" = {
          type = "\"custom/text\"";

          click-left = "\"rofi-power-menu\"";

          format = "\"${config.home.username} %{T2}󰍃%{T-} \"";
          format-background = "\"${darkest}\"";
          format-foreground = "\"${light}\"";
        };
        # "module/mode" = {
        #   type = "\"custom/ipc\"";
        #   # startup
        #   initial = "\"1\"";
        #   # move
        #   hook-0 = "\"\"";
        #   format-0 = "\" %{O-1}%{T2}󰆾%{T-}\"";
        #   format-0-background = "\"${light}\"";
        #   format-0-foreground = "\"${dark}\"";
        #   format-0-padding = "\"0\"";
        #   # resize
        #   hook-1 = "\"\"";
        #   format-1 = "\" %{O1}%{T2}󰁌%{T-}\"";
        #   format-1-background = "\"${light}\"";
        #   format-1-foreground = "\"${dark}\"";
        #   format-1-padding = "\"0\"";
        # };
        # "module/keyboard" = {
        #   type = "\"internal/xkeyboard\"";

        #   # REVIEW: not working
        #   # click-left = "\"${pkgs.numlockx}/bin/numlockx toggle &\"";
        #   blacklist-0 = "\"caps lock\"";
        #   blacklist-1 = "\"scroll lock\"";

        #   format-foreground = "\"${dark}\"";
        #   format-background = "\"${light}\"";

        #   # label-indicator-off-capslock = "\" %{T2}󰯫%{T-}\"";
        #   # label-indicator-off-capslock-foreground = "\"${dark}\"";
        #   # label-indicator-on-capslock = "\" %{T2}󰬈%{T-}\"";
        #   # label-indicator-on-capslock-foreground = "\"${dark}\"";

        #   label-indicator-off-numlock = "\"%{T2}󰰒%{T-}\"";
        #   label-indicator-off-numlock-foreground = "\"${dark}\"";
        #   label-indicator-on-numlock = "\"%{T2}󰬕%{T-}\"";
        #   label-indicator-on-numlock-foreground = "\"${dark}\"";

        #   # label-indicator-off-scrolllock = "\" %{T2}󰰡%{T-}\"";
        #   # label-indicator-off-scrolllock-foreground = "\"${dark}\"";
        #   # label-indicator-on-scrolllock = "\" %{T2}󰬚%{T-}\"";
        #   # label-indicator-on-scrolllock-foreground = "\"${dark}\"";
        #   label-layout = "\"\"";
        #   # label-layout-padding = "\"0\"";
        # };
        "module/toggle" = {
          type = "\"custom/ipc\"";
          click-left = "\"${toggle} toggle notifications tray &\"";
          scroll-down = "\"${toggle} toggle notifications tray &\"";
          scroll-up = "\"${toggle} toggle notifications tray &\"";
          initial = "\"1\"";
          hook-0 = "\"\"";
          format-0 = "\"%{T2}%{T-}\"";
          format-0-foreground = "\"${light}\"";
          format-0-background = "\"${dark}\"";
          format-0-padding = "\"1\"";
          hook-1 = "\"\"";
          format-1 = "\"%{T2}%{T-}\"";
          format-1-foreground = "\"${darkest}\"";
          format-1-background = "\"${dark}\"";
          format-1-padding = "\"1\"";
        };
        "module/notifications" = {
          type = "\"custom/ipc\"";
          hidden = "\"true\"";
          click-left = "\"${dunst-toggle} &\"";
          click-right = "\"rofi-dunst &\"";
          scroll-down = "\"${dunstctl} history-pop &\"";
          scroll-up = "\"${dunstctl} close &\"";
          initial = "\"1\"";
          hook-0 = "\"\"";
          format-0 = "\"%{O-12}%{T2}%{T-}\"";
          format-0-foreground = "\"${light}\"";
          format-0-background = "\"${dark}\"";
          format-0-padding = "\"1\"";
          hook-1 = "\"\"";
          format-1 = "\"%{O-12}%{T2}%{T-}\"";
          format-1-foreground = "\"${light}\"";

          format-1-background = "\"${dark}\"";
          format-1-padding = "\"1\"";
        };
        "module/tray" = {
          type = "\"internal/tray\"";
          hidden = "\"true\"";
          tray-spacing = "\"1pt\"";
          tray-padding = "\"1pt\"";
          format = "\"%{O-2}<tray>\"";
          format-background = "\"${dark}\"";
          tray-foreground = "\"${light}\"";
          tray-background = "\"${dark}\"";
          tray-size = "\"75%\"";
        };
        "module/title" = {
          type = "\"internal/xwindow\"";
          label = "\"%title:0:64:...%\"";
          label-background = "\"${dark}\"";
          label-empty = "\"Desktop\"";
          label-empty-background = "\"${dark}\"";
        };
        "module/filesystem" = {
          type = "\"internal/fs\"";
          warn-percentage = "\"50\"";
          mount-0 = "\"/\"";

          format-mounted = "\"<label-mounted> %{T2}󰋊%{T-} \"";
          format-mounted-foreground = "\"${light}\"";
          format-mounted-background = "\"${dark}\"";
          label-mounted = "\"%free%\"";

          format-warn = "\"<label-warn>\"";
          format-warn-foreground = "\"${light}\"";
          format-warn-background = "\"${dark}\"";
          label-warn = "\"%free%\"";

          format-unmounted = "\"<label-unmounted> %{T2}󰋊%{T-} \"";
          format-unmounted-foreground = "\"${light}\"";
          format-unmounted-background = "\"${dark}\"";
          label-unmounted = "\"?\"";
        };
        "module/temperature" = {
          type = "\"internal/temperature\"";
          interval = "\"0.5\"";
          thermal-zone = "\"0\"";
          zone-type = "\"x86_pkg_temp\"";
          base-temperature = "\"20\"";
          warn-temperature = "\"80\"";
          ramp-0 = "\"\"";
          ramp-1 = "\"\"";
          ramp-2 = "\"\"";
          ramp-3 = "\"\"";
          ramp-4 = "\"\"";

          format = "\"<label> %{T2}<ramp>%{T-} \"";
          format-foreground = "\"${dark}\"";
          format-background = "\"${light}\"";
          label = "\"%temperature-c%\"";

          format-warn = "\"<label-warn> %{T2}<ramp>%{T-} \"";
          format-warn-foreground = "\"${dark}\"";
          format-warn-background = "\"${light}\"";
          label-warn = "\"%temperature-c%\"";
        };
        "module/memory" = {
          type = "\"internal/memory\"";

          format = "\"<label> %{T2}%{T-} \"";
          format-foreground = "\"${light}\"";
          format-background = "\"${dark}\"";
          label = "\"%percentage_used%%\"";

          format-warn = "\"<label-warn> %{T2}%{T-} \"";
          format-warn-foreground = "\"${light}\"";
          format-warn-background = "\"${dark}\"";
          label-warn = "\"%percentage_used%%\"";
        };
        "module/cpu" = {
          type = "\"internal/cpu\"";
          interval = "\"0.5\"";

          ramp-load-0 = "\"▁\"";
          ramp-load-1 = "\"▂\"";
          ramp-load-2 = "\"▃\"";
          ramp-load-3 = "\"▄\"";
          ramp-load-4 = "\"▅\"";
          ramp-load-5 = "\"▆\"";
          ramp-load-6 = "\"▇\"";
          ramp-load-7 = "\"█\"";
          warn-percentage = "\"50\"";

          format = "\"<ramp-load> %{T2}%{T-} \"";
          format-foreground = "\"${dark}\"";
          format-background = "\"${light}\"";

          format-warn = "\"%{F${danger}}<ramp-load>%{F${danger}} %{T2}%{T-} \"";
          format-warn-foreground = "\"${dark}\"";
          format-warn-background = "\"${light}\"";
        };
        "module/wired" = {
          type = "\"internal/network\"";
          interface-type = "\"wired\"";
          interval = "\"10\"";

          format-connected = "\"<label-connected>\"";
          format-connected-foreground = "\"${light}\"";
          format-connected-background = "\"${dark}\"";
          label-connected = "\"%netspeed% %{T2}󰈁%{T-} \"";

          format-disconnected = "\"<label-disconnected>\"";
          format-disconnected-foreground = "\"${light}\"";
          format-disconnected-background = "\"${dark}\"";
          label-disconnected = "\"off  %{T2}󰈂{T-} \"";
        };
        "module/wireless" = {
          type = "\"internal/network\"";
          interface-type = "\"wireless\"";
          interval = "\"2\"";

          ramp-signal-0 = "\"󰤯\"";
          ramp-signal-1 = "\"󰤟\"";
          ramp-signal-2 = "\"󰤢\"";
          ramp-signal-3 = "\"󰤥\"";
          ramp-signal-4 = "\"󰤨\"";

          format-connected = "\"<label-connected> %{T2}<ramp-signal>%{T-} \"";
          format-connected-foreground = "\"${dark}\"";
          format-connected-background = "\"${light}\"";
          label-connected = "\"%signal%%\"";

          format-disconnected = "\"<label-disconnected> %{T2}󰤠{T-} \"";
          format-disconnected-foreground = "\"${dark}\"";
          format-disconnected-background = "\"${light}\"";
          label-disconnected = "\"off\"";
        };
        "module/bluetooth" = {
          # TODO: install bluez on user level https://bbs.archlinux.org/viewtopic.php?id=215080
          type = "\"custom/ipc\"";
          click-left = "\"${bluetooth} --toggle &\"";
          click-right = "\"${pkgs.libsForQt5.bluedevil}/bin/bluedevil-wizard &\"";
          # startup
          initial = "\"1\"";
          hook-0 = "\"${bluetooth} &\"";
          label = "\"%output% \"";
          # off
          hook-1 = "\"\"";
          format-1 = "\"%{T2}󰂲%{T-} \"";
          format-1-foreground = "\"${light}\"";
          format-1-background = "\"${dark}\"";
          # on
          hook-2 = "\"\"";
          format-2 = "\"%{T2}%{T-} \"";
          format-2-foreground = "\"${light}\"";
          format-2-background = "\"${dark}\"";

          # TODO: https://github.com/polybar/polybar-scripts/blob/master/polybar-scripts/system-usb-udev/system-usb-udev.sh
        };
        "module/audio" = {
          type = "\"custom/ipc\"";
          initial = "\"1\"";

          click-left = "\"${pipewire} --mute &\"";
          click-right = "\"${pkgs.lxqt.pavucontrol-qt}/bin/pavucontrol-qt &\"";
          scroll-down = "\"${pipewire} --down &\"";
          scroll-up = "\"${pipewire} --up &\"";

          # startup
          hook-0 = "\"${pipewire} &\"";
          label = "\"%output%%\"";

          # mute
          format-1 = "\"<label> %{T2}󰝟%{T-} \"";
          format-1-background = "\"${light}\"";
          format-1-foreground = "\"${dark}\"";
          hook-1 = "\"${pamixer} --get-volume\"";

          # low volume
          format-2 = "\"<label> %{T2}󰕾%{T-} \"";
          format-2-background = "\"${light}\"";
          format-2-foreground = "\"${dark}\"";
          hook-2 = "\"${pamixer} --get-volume\"";

        };
        # TODO: replace with custom module to support calendar
        "module/date" = {
          type = "\"custom/script\"";
          interval = "\"1.0\"";
          # posx: 3264 + 1280
          click-left = "\"${pkgs.yad}/bin/yad --calendar --undecorated --fixed --close-on-unfocus --no-buttons --posx=4544 --posy=36 &\"";
          exec = "\"echo \"$(${pkgs.toybox}/bin/date +'%a, %d %b %Y') %{T2}󰃭%{T-}\"\"";

          label = "\"%output% \"";
          label-background = "\"${dark}\"";
          label-foreground = "\"${light}\"";
          label-padding-right = "\"1\"";
        };
        "module/time" = {
          type = "\"internal/date\"";
          interval = "\"1.0\"";
          time = "\"%H:%M:%S %{T2}󰥔%{T-}\"";

          label = "\"%time% \"";
          format-background = "\"${light}\"";
          format-foreground = "\"${dark}\"";
        };
        "module/menu-r" = {
          type = "\"custom/text\"";
          label-background = "\"${light}\"";
          label-foreground = "\"${darkest}\"";
          label = "\"%{T3}%{T-}%{O-1}\"";
        };
        "module/menu-l" = {
          type = "\"custom/text\"";
          label = "\"%{O-1}%{O1}%{B${light}}%{T3} %{T-}%{B-}%{O-25}%{T3}%{T-}\"";
          label-foreground = "\"${darkest}\"";
        };
        "module/left" = {
          type = "\"custom/text\"";
          label = "\"%{F${dark}}%{T3}%{T-}%{F-}%{O-12}%{F${darkest}}%{T3}%{T-}%{F-}%{O-12}%{F${dark}}%{T3}%{T-}%{F-}\"";
          format = "\"<label>%{O-2}\"";
        };
        "module/right" = {
          type = "\"custom/text\"";
          label = "\"%{O23}%{F${dark}}%{T3}%{T-}%{F-}%{F-}%{O-36}%{F${darkest}}%{T3}%{T-}%{F-}%{O-36}%{F${dark}}%{T3}%{T-}\"";
          format = "\"<label>%{O-2}\"";
        };
        "module/even-l" = {
          type = "\"custom/text\"";
          label = "\"%{O-1}%{O1}%{B${dark}}%{T3} %{T-}%{B-}%{O-25}%{T3}%{T-}\"";
          label-foreground = "\"${light}\"";
        };
        "module/even-r" = {
          type = "\"custom/text\"";
          label-background = "\"${dark}\"";
          label-foreground = "\"${light}\"";
          label = "\"%{T3}%{T-}%{O-1}\"";
        };
        "module/odd-r" = {
          type = "\"custom/text\"";
          label-background = "\"${light}\"";
          label-foreground = "\"${dark}\"";
          label = "\"%{T3}%{T-}%{O-1}\"";
        };
      };
    };
  };
}
