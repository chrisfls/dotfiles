{ config, lib, pkgs, specialArgs, ... }:
let
  cfg = config.extra.polybar;

  invisible = "#00000000";

  colors = specialArgs.color-schemes.popping-and-locking;

  polybar-msg = "${pkgs.polybar}/bin/polybar-msg";

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

  bluetoothctl = "${pkgs.bluez}/bin/bluetoothctl";

  bluetooth =
    let
      script = pkgs.writeShellScriptBin "polybar-bluetooth"
        ''
          if ${bluetoothctl} show | grep -q "Powered: yes"; then
              hook=2
          else
              hook=1
          fi

          if [ "$1" = "--toggle" ]; then
              if [ "$hook" = "2" ]; then
                  ${bluetoothctl} power off
                  hook=1
              else
                  ${bluetoothctl} power on
                  hook=2
              fi
          fi


          ${polybar-msg} action bluetooth hook $hook
        '';
    in
    "${script}/bin/polybar-bluetooth";

  pamixer = "${pkgs.pamixer}/bin/pamixer";

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
in
{
  options.extra.polybar = {
    enable = lib.mkEnableOption "Enable polybar module";
    color-scheme = lib.mkOption {
      type = lib.types.attrs;
      default = specialArgs.color-schemes.popping-and-locking-black;
    };
  };

  config = lib.mkIf cfg.enable {
    systemd.user.services.sxhkd-chord-mode = {
      Unit.Description = "Trigger sxhkd chord indicator";
      Service.ExecStart =
        let
          script = pkgs.writeShellScriptBin "polybar-sxhkd"
            ''
              cat $SXHKD_FIFO | while read -r line; do
                  echo $line
                  if [[ $line == *"BBegin chain"* ]]; then
                      ${polybar-msg} action sxhkd hook 1
                  elif [[ $line == *"EEnd chain"* ]]; then
                      ${polybar-msg} action sxhkd hook 0
                  fi
              done
            '';
        in
        "${script}/bin/polybar-sxhkd";
    };

    services.polybar = {
      enable = true;
      script = "polybar -r &";
      settings = {
        "settings" = {
          pseudo-transparency = "\"true\"";
        };
        "bar/topbar" = {
          # settings
          enable-ipc = "\"true\"";
          wm-restack = "\"bspwm\"";

          # layout
          dpi = "\"144\"";
          height = "\"18pt\"";
          width = "\"100%\"";
          line-size = "\"2pt\"";
          padding-left = "\"0\"";
          padding-right = "\"0\"";
          dim-value = "\"0.5\"";

          # styling

          # base font
          font-0 = "\"CaskaydiaCoveNerdFontMono:size=10;4\"";

          # icon font
          font-1 = "\"CaskaydiaCoveNerdFontPropo:size=11;3\"";

          # powerline font
          font-2 = "\"CaskaydiaCoveNerdFont:size=20;6\"";

          background = "\"${invisible}\"";
          foreground = "\"${colors.foreground}\"";

          # modules
          modules-left = "\"mode keyboard mode-keyboard-workspaces workspaces toggle tray workspaces-toggle-tray\"";
          modules-center = "\"title-left title title-right\"";
          modules-right = "\"title-filesystem filesystem filesystem-temperature temperature temperature-memory memory memory-cpu cpu cpu-wired wired wired-wireless wireless wireless-bluetooth bluetooth bluetooth-audio audio audio-date date date-time time\"";
          format-radius = "\"32.0\"";
        };
        "module/mode" = {
          type = "\"custom/ipc\"";
          # startup
          initial = "\"1\"";
          # move
          hook-0 = "\"\"";
          format-0 = "\"%{T2}󰆾%{T-}\"";
          format-0-background = "\"${colors.foreground}\"";
          format-0-foreground = "\"${colors.background}\"";
          format-0-padding = "\"1\"";
          # resize
          hook-1 = "\"\"";
          format-1 = "\"%{T2}󰩨%{T-}\"";
          format-1-background = "\"${colors.foreground}\"";
          format-1-foreground = "\"${colors.background}\"";
          format-1-padding = "\"1\"";

        };
        "module/keyboard" = {
          type = "\"internal/xkeyboard\"";
          
          # REVIEW: not working
          # click-left = "\"${pkgs.numlockx}/bin/numlockx toggle &\"";
          blacklist-0 = "\"caps lock\"";
          blacklist-1 = "\"scroll lock\"";

          format-foreground = "\"${colors.background}\"";
          format-background = "\"${colors.foreground}\"";

          label-indicator-off-capslock = "\"%{T2}󰯫%{T-} \"";
          label-indicator-off-capslock-foreground = "\"${colors.background}\"";
          label-indicator-off-numlock = "\"%{T2}󰰒%{T-} \"";
          label-indicator-off-numlock-foreground = "\"${colors.background}\"";
          label-indicator-off-scrolllock = "\"%{T2}󰰡%{T-} \"";
          label-indicator-off-scrolllock-foreground = "\"${colors.background}\"";
          label-indicator-on-capslock = "\"%{T2}󰬈%{T-} \"";
          label-indicator-on-capslock-foreground = "\"${colors.background}\"";
          label-indicator-on-numlock = "\"%{T2}󰬕%{T-} \"";
          label-indicator-on-numlock-foreground = "\"${colors.background}\"";
          label-indicator-on-scrolllock = "\"%{T2}󰬚%{T-} \"";
          label-indicator-on-scrolllock-foreground = "\"${colors.background}\"";
          label-layout = "\"\"";
        };
        "module/mode-keyboard-workspaces" = {
          type = "\"custom/text\"";
          label-foreground = "\"${colors.foreground}\"";
          label = "\"%{B${colors.background}} %{B-}%{O-25}%{T3}%{T-}\"";
          format = "\"<label>%{O-2}\"";
        };
        "module/workspaces" = {
          type = "\"internal/bspwm\"";

          pin-workspaces = "\"true\"";
          enable-scroll = "\"true\"";
          reverse-scroll = "\"false\"";
          fuzzy-match = "\"false\"";
          occupied-scroll = "\"true\"";

          format = "\"%{O1}%{T1}%{O2}<label-state>%{T-}\"";
          label-active="";
          label-active-background = "\"${colors.background}\"";
          label-active-foreground = "\"${colors.foreground}\"";
          label-active-padding = "\"1\"";

          label-empty = "\"\"";
          label-empty-background = "\"${colors.background}\"";
          label-empty-foreground = "\"${colors.blackBright}\"";
          label-empty-padding = "\"1\"";

          label-occupied = "\"\"";
          label-occupied-background = "\"${colors.background}\"";
          label-occupied-foreground = "\"${colors.blackBright}\"";
          label-occupied-padding = "\"1\"";

          label-urgent = "\"\"";
          label-urgent-background = "\"${colors.background}\"";
          label-urgent-foreground = "\"${colors.yellow}\"";
          label-urgent-padding = "\"1\"";
        };
        "module/toggle" = {
          type = "\"custom/ipc\"";
          scroll-down = "\"${toggle} toggle tray &\"";
          scroll-up = "\"${toggle} toggle tray &\"";
          initial = "\"1\"";
          hook-0 = "\"\"";
          format-0 = "\"%{T2}%{T-}\"";
          format-0-foreground = "\"${colors.foreground}\"";
          format-0-background = "\"${colors.background}\"";
          format-0-padding = "\"1\"";
          hook-1 = "\"\"";
          format-1 = "\"%{T2}%{T-}\"";
          format-1-foreground = "\"${colors.foreground}\"";
          format-1-background = "\"${colors.background}\"";
          format-1-padding = "\"1\"";
        };
        "module/tray" = {
          type = "\"internal/tray\"";
          tray-spacing = "\"1pt\"";
          tray-padding = "\"1pt\"";
          format = "\"%{O-2}<tray>\"";
          format-background = "\"${colors.background}\"";
          tray-foreground = "\"${colors.foreground}\"";
          tray-background = "\"${colors.background}\"";
          tray-size = "\"100%\"";
        };
        "module/workspaces-toggle-tray" = {
          type = "\"custom/text\"";
          label-foreground = "\"${colors.background}\"";
          label-background = "\"${invisible}\"";
          label = "\"%{O-2}%{T3}%{T-}\"";
          format = "\"<label>%{O-3}\"";
        };
        "module/title-left" = {
          type = "\"custom/text\"";
          label-foreground = "\"${colors.background}\"";
          label-background = "\"${invisible}\"";
          label = "\"%{T3}%{T-}\"";
          format = "\"<label>%{O-2}\"";
        };
        "module/title" = {
          type = "\"internal/xwindow\"";
          label = "\"%title:0:64:...%\"";
          label-background = "\"${colors.background}\"";
          label-empty = "\"Desktop\"";
          label-empty-background = "\"${colors.background}\"";
        };
        "module/title-right" = {
          type = "\"custom/text\"";
          label-foreground = "\"${colors.background}\"";
          label-background = "\"${invisible}\"";
          label = "\"%{T3}%{T-}\"";
          format = "\"<label>%{O-3}\"";
        };
        "module/title-filesystem" = {
          type = "\"custom/text\"";
          label-foreground = "\"${colors.magentaBright}\"";
          label = "\"%{T3}%{T-}\"";
          format = "\"<label>%{O-2}\"";
        };
        "module/filesystem" = {
          type = "\"internal/fs\"";
          warn-percentage = "\"50\"";
          mount-0 = "\"/\"";

          format-mounted = "\"<label-mounted> %{T2}󰋊%{T-}  \"";
          format-mounted-foreground = "\"${colors.background}\"";
          format-mounted-background = "\"${colors.magentaBright}\"";
          label-mounted = "\"%free%\"";

          format-warn = "\"<label-warn>\"";
          format-warn-foreground = "\"${colors.background}\"";
          format-warn-background = "\"${colors.magentaBright}\"";
          label-warn = "\"%free%\"";

          format-unmounted = "\"<label-unmounted> %{T2}󰋊%{T-}  \"";
          format-unmounted-foreground = "\"${colors.background}\"";
          format-unmounted-background = "\"${colors.magenta}\"";
          label-unmounted = "\"?\"";
        };
        "module/filesystem-temperature" = {
          type = "\"custom/text\"";
          label-foreground = "\"${colors.redBright}\"";
          label = "\"%{B${colors.magentaBright}} %{B-}%{O-18}%{T3}%{T-}\"";
          format = "\"<label>%{O-2}\"";
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

          format = "\"<label> %{T2}<ramp>%{T-}  \"";
          format-foreground = "\"${colors.black}\"";
          format-background = "\"${colors.redBright}\"";
          label = "\"%temperature-c%\"";

          format-warn = "\"<label-warn> %{T2}<ramp>%{T-}  \"";
          format-warn-foreground = "\"${colors.foreground}\"";
          format-warn-background = "\"${colors.redBright}\"";
          label-warn = "\"%temperature-c%\"";
        };
        "module/temperature-memory" = {
          type = "\"custom/text\"";
          label-foreground = "\"${colors.yellowBright}\"";
          label = "\"%{B${colors.redBright}} %{B-}%{O-18}%{T3}%{T-}\"";
          format = "\"<label>%{O-2}\"";

        };
        "module/memory" = {
          type = "\"internal/memory\"";

          format = "\"<label> %{T2}%{T-} \"";
          format-foreground = "\"${colors.background}\"";
          format-background = "\"${colors.yellowBright}\"";
          label = "\"%percentage_used%%\"";

          format-warn = "\"<label> %{T2}%{T-} \"";
          format-warn-foreground = "\"${colors.background}\"";
          format-warn-background = "\"${colors.yellowBright}\"";
          label-warn = "\"%percentage_used%%\"";
        };
        "module/memory-cpu" = {
          type = "\"custom/text\"";
          label-foreground = "\"${colors.greenBright}\"";
          label = "\"%{B${colors.yellowBright}} %{B-}%{O-8}%{T3}%{T-}\"";
          format = "\"<label>%{O-2}\"";
        };
        "module/cpu" = {
          type = "\"internal/cpu\"";
          interval = "\"0.25\"";

          ramp-load-0 = "\"▁\"";
          ramp-load-1 = "\"▂\"";
          ramp-load-2 = "\"▃\"";
          ramp-load-3 = "\"▄\"";
          ramp-load-4 = "\"▅\"";
          ramp-load-5 = "\"▆\"";
          ramp-load-6 = "\"▇\"";
          ramp-load-7 = "\"█\"";
          warn-percentage = "\"50\"";

          format = "\"<ramp-load> %{T2}%{T-}  \"";
          format-foreground = "\"${colors.background}\"";
          format-background = "\"${colors.greenBright}\"";

          format-warn = "\"%{F#CC241D}<ramp-load>%{F${colors.background}} %{T2}%{T-} \"";
          format-warn-foreground = "\"${colors.background}\"";
          format-warn-background = "\"${colors.greenBright}\"";
        };
        "module/cpu-wired" = {
          type = "\"custom/text\"";
          label-foreground = "\"${colors.cyanBright}\"";
          label = "\"%{B${colors.greenBright}} %{B-}%{O-18}%{T3}%{T-}\"";
          format = "\"<label>%{O-2}\"";
        };
        "module/wired" = {
          type = "\"internal/network\"";
          interface-type = "\"wired\"";
          interval = "\"1\"";

          format-connected = "\"<label-connected>\"";
          format-connected-foreground = "\"${colors.black}\"";
          format-connected-background = "\"${colors.cyanBright}\"";
          label-connected = "\"%netspeed% %{T2}󰈁%{T-} \"";

          format-disconnected = "\"<label-disconnected>\"";
          format-disconnected-foreground = "\"${colors.black}\"";
          format-disconnected-background = "\"${colors.cyanBright}\"";
          label-disconnected = "\"down  %{T2}󰈂{T-} \"";
        };
        "module/wired-wireless" = {
          type = "\"custom/text\"";
          label-background = "\"${colors.cyanBright}\"";
          label-foreground = "\"${colors.cyan}\"";
          label = "\"%{T3}%{T-}\"";
        };
        "module/wireless" = {
          type = "\"internal/network\"";
          interface-type = "\"wireless\"";
          interval = "\"1\"";

          ramp-signal-0 = "\"󰤯\"";
          ramp-signal-1 = "\"󰤟\"";
          ramp-signal-2 = "\"󰤢\"";
          ramp-signal-3 = "\"󰤥\"";
          ramp-signal-4 = "\"󰤨\"";

          format-connected = "\"<label-connected> %{T2}<ramp-signal>%{T-}  \"";
          format-connected-foreground = "\"${colors.black}\"";
          format-connected-background = "\"${colors.cyanBright}\"";
          label-connected = "\" %signal%%\"";

          format-disconnected = "\"<label-disconnected> %{T2}󰤠{T-}  \"";
          format-disconnected-foreground = "\"${colors.black}\"";
          format-disconnected-background = "\"${colors.cyanBright}\"";
          label-disconnected = "\" off\"";
        };
        "module/wireless-bluetooth" = {
          type = "\"custom/text\"";
          label-foreground = "\"${colors.blueBright}\"";
          label = "\"%{B${colors.cyanBright}} %{B-}%{O-18}%{T3}%{T-}\"";
          format = "\"<label>%{O-2}\"";
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
          format-1-foreground = "\"${colors.blue}\"";
          format-1-background = "\"${colors.blueBright}\"";
          # on
          hook-2 = "\"\"";
          format-2 = "\"%{T2}%{T-} \"";
          format-2-foreground = "\"${colors.background}\"";
          format-2-background = "\"${colors.blueBright}\"";

          # TODO: https://github.com/polybar/polybar-scripts/blob/master/polybar-scripts/system-usb-udev/system-usb-udev.sh
        };
        "module/bluetooth-audio" = {
          type = "\"custom/text\"";
          label-background = "\"${colors.blueBright}\"";
          label-foreground = "\"${colors.blue}\"";
          label = "\"%{T3}%{T-}\"";

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
          label = "\" %output%%\"";

          # mute
          format-1 = "\"<label> %{T2}󰝟%{T-}  \"";
          format-1-background = "\"${colors.blueBright}\"";
          format-1-foreground = "\"${colors.blue}\"";
          hook-1 = "\"${pamixer} --get-volume\"";

          # low volume
          format-2 = "\"<label> %{T2}󰕾%{T-}  \"";
          format-2-background = "\"${colors.blueBright}\"";
          format-2-foreground = "\"${colors.background}\"";
          hook-2 = "\"${pamixer} --get-volume\"";

        };
        "module/audio-date" = {
          type = "\"custom/text\"";
          label-foreground = "\"${colors.background}\"";
          label = "\"%{B${colors.blueBright}} %{B-}%{O-18}%{T3}%{T-}%{O-1}\"";
          format = "\"<label>\"";
        };
        "module/date" = {
          type = "\"internal/date\"";
          interval = "\"1.0\"";
          date = "\"%a, %d %b %Y %{T2}󰃭%{T-}\"";

          # REVIEW: not working
          # click-left = "\"${pkgs.yad}/bin/yad --calendar --undecorated --fixed --close-on-unfocus --no-buttons &\"";

          label = "\"%date% \"";
          label-background = "\"${colors.background}\"";
          label-foreground = "\"${colors.foreground}\"";
          label-padding-right = "\"1\"";

        };
        "module/date-time" = {
          type = "\"custom/text\"";
          label-background = "\"${colors.background}\"";
          label-foreground = "\"${colors.foreground}\"";
          label = "\"%{T3}%{T-}%{O-1}\"";
        };
        "module/time" = {
          type = "\"internal/date\"";
          interval = "\"1.0\"";
          time = "\"%H:%M:%S %{T2}󰥔%{T-}\"";

          label = "\"%time% \"";
          format-background = "\"${colors.foreground}\"";
          format-foreground = "\"${colors.background}\"";
        };
      };
    };
  };
}

