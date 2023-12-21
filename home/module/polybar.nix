{ config, lib, pkgs, specialArgs, ... }:
let
  cfg = config.extra.polybar;

  colors = config.extra.polybar.color-scheme;

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

  foreground = colors.foreground;

  background = colors.background;

  color = colors.blue;
in
{
  options.extra.polybar = {
    enable = lib.mkEnableOption "Enable polybar module";
    color-scheme = lib.mkOption {
      type = lib.types.attrs;
      default = specialArgs.color-schemes.popping-and-locking;
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
                      ${polybar-msg} action mode hook 1
                  elif [[ $line == *"EEnd chain"* ]]; then
                      ${polybar-msg} action mode hook 0
                  fi
              done
            '';
        in
        "${script}/bin/polybar-sxhkd";
    };

    services.polybar = {
      enable = true;
      script = "polybar &";
      settings = {
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

          background = "\"${background}\"";
          foreground = "\"${foreground}\"";

          # modules
          modules-left = "\"mode keyboard even-l workspaces toggle tray right\"";
          modules-center = "\"left title right \"";
          modules-right = "\"left filesystem even-r temperature odd-r memory even-r cpu odd-r wired even-r wireless odd-r bluetooth even-r audio odd-r date even-r time\"";
          format-radius = "\"32.0\"";
        };
        "module/mode" = {
          type = "\"custom/ipc\"";
          # startup
          initial = "\"1\"";
          # move
          hook-0 = "\"\"";
          format-0 = "\"%{T2}󰆾%{T-}\"";
          format-0-background = "\"${foreground}\"";
          format-0-foreground = "\"${color}\"";
          format-0-padding = "\"1\"";
          # resize
          hook-1 = "\"\"";
          format-1 = "\"%{T2}󰩨%{T-}%{O1}\"";
          format-1-background = "\"${foreground}\"";
          format-1-foreground = "\"${color}\"";
          format-1-padding = "\"1\"";

        };
        "module/keyboard" = {
          type = "\"internal/xkeyboard\"";
          
          # REVIEW: not working
          # click-left = "\"${pkgs.numlockx}/bin/numlockx toggle &\"";
          blacklist-0 = "\"caps lock\"";
          blacklist-1 = "\"scroll lock\"";

          format-foreground = "\"${color}\"";
          format-background = "\"${foreground}\"";

          label-indicator-off-capslock = "\"%{T2}󰯫%{T-} \"";
          label-indicator-off-capslock-foreground = "\"${color}\"";
          label-indicator-off-numlock = "\"%{T2}󰰒%{T-} \"";
          label-indicator-off-numlock-foreground = "\"${color}\"";
          label-indicator-off-scrolllock = "\"%{T2}󰰡%{T-} \"";
          label-indicator-off-scrolllock-foreground = "\"${color}\"";
          label-indicator-on-capslock = "\"%{T2}󰬈%{T-} \"";
          label-indicator-on-capslock-foreground = "\"${color}\"";
          label-indicator-on-numlock = "\"%{T2}󰬕%{T-} \"";
          label-indicator-on-numlock-foreground = "\"${color}\"";
          label-indicator-on-scrolllock = "\"%{T2}󰬚%{T-} \"";
          label-indicator-on-scrolllock-foreground = "\"${color}\"";
          label-layout = "\"\"";
        };
        "module/mode-keyboard-workspaces" = {
          type = "\"custom/text\"";
          label-foreground = "\"${color}\"";
          label = "\"%{B${color}} %{B-}%{O-25}%{T3}%{T-}\"";
          format = "\"<label>%{O-2}\"";
        };
        "module/workspaces" = {
          type = "\"internal/bspwm\"";

          pin-workspaces = "\"true\"";
          enable-scroll = "\"true\"";
          reverse-scroll = "\"false\"";
          fuzzy-match = "\"false\"";
          occupied-scroll = "\"true\"";

          format = "\"%{O-1}%{T1}%{O2}<label-state>%{T-}\"";
          label-active="";
          label-active-background = "\"${color}\"";
          label-active-foreground = "\"${foreground}\"";
          label-active-padding = "\"1\"";

          label-empty = "\"\"";
          label-empty-background = "\"${color}\"";
          label-empty-foreground = "\"${background}\"";
          label-empty-padding = "\"1\"";

          label-occupied = "\"\"";
          label-occupied-background = "\"${color}\"";
          label-occupied-foreground = "\"${background}\"";
          label-occupied-padding = "\"1\"";

          label-urgent = "\"\"";
          label-urgent-background = "\"${color}\"";
          label-urgent-foreground = "\"${colors.yellow}\"";
          label-urgent-padding = "\"1\"";
        };
        "module/toggle" = {
          type = "\"custom/ipc\"";
          scroll-down = "\"${toggle} toggle tray &\"";
          scroll-up = "\"${toggle} toggle tray &\"";
          initial = "\"1\"";
          hook-0 = "\"\"";
          format-0 = "\"%{T2}%{T-}\"";
          format-0-foreground = "\"${background}\"";
          format-0-background = "\"${color}\"";
          format-0-padding = "\"1\"";
          hook-1 = "\"\"";
          format-1 = "\"%{T2}%{T-}\"";
          format-1-foreground = "\"${foreground}\"";
          format-1-background = "\"${color}\"";
          format-1-padding = "\"1\"";
        };
        "module/tray" = {
          type = "\"internal/tray\"";
          tray-spacing = "\"1pt\"";
          tray-padding = "\"1pt\"";
          format = "\"%{O-2}<tray>\"";
          format-background = "\"${color}\"";
          tray-foreground = "\"${foreground}\"";
          tray-background = "\"${color}\"";
          tray-size = "\"100%\"";
        };
        "module/title" = {
          type = "\"internal/xwindow\"";
          label = "\"%title:0:64:...%\"";
          label-background = "\"${color}\"";
          label-empty = "\"Desktop\"";
          label-empty-background = "\"${color}\"";
        };
        "module/filesystem" = {
          type = "\"internal/fs\"";
          warn-percentage = "\"50\"";
          mount-0 = "\"/\"";

          format-mounted = "\"<label-mounted> %{T2}󰋊%{T-} \"";
          format-mounted-foreground = "\"${foreground}\"";
          format-mounted-background = "\"${color}\"";
          label-mounted = "\"%free%\"";

          format-warn = "\"<label-warn>\"";
          format-warn-foreground = "\"${foreground}\"";
          format-warn-background = "\"${color}\"";
          label-warn = "\"%free%\"";

          format-unmounted = "\"<label-unmounted> %{T2}󰋊%{T-} \"";
          format-unmounted-foreground = "\"${foreground}\"";
          format-unmounted-background = "\"${color}\"";
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
          format-foreground = "\"${color}\"";
          format-background = "\"${foreground}\"";
          label = "\"%temperature-c%\"";

          format-warn = "\"<label-warn> %{T2}<ramp>%{T-} \"";
          format-warn-foreground = "\"${color}\"";
          format-warn-background = "\"${foreground}\"";
          label-warn = "\"%temperature-c%\"";
        };
        "module/memory" = {
          type = "\"internal/memory\"";

          format = "\"<label> %{T2}%{T-} \"";
          format-foreground = "\"${foreground}\"";
          format-background = "\"${color}\"";
          label = "\"%percentage_used%%\"";

          format-warn = "\"<label> %{T2}%{T-} \"";
          format-warn-foreground = "\"${foreground}\"";
          format-warn-background = "\"${color}\"";
          label-warn = "\"%percentage_used%%\"";
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

          format = "\"<ramp-load> %{T2}%{T-} \"";
          format-foreground = "\"${color}\"";
          format-background = "\"${foreground}\"";

          format-warn = "\"%{F${colors.red}}<ramp-load>%{F${colors.red}} %{T2}%{T-} \"";
          format-warn-foreground = "\"${color}\"";
          format-warn-background = "\"${foreground}\"";
        };
        "module/wired" = {
          type = "\"internal/network\"";
          interface-type = "\"wired\"";
          interval = "\"1\"";

          format-connected = "\"<label-connected>\"";
          format-connected-foreground = "\"${foreground}\"";
          format-connected-background = "\"${color}\"";
          label-connected = "\"%netspeed% %{T2}󰈁%{T-} \"";

          format-disconnected = "\"<label-disconnected>\"";
          format-disconnected-foreground = "\"${foreground}\"";
          format-disconnected-background = "\"${color}\"";
          label-disconnected = "\"down  %{T2}󰈂{T-} \"";
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

          format-connected = "\"<label-connected> %{T2}<ramp-signal>%{T-} \"";
          format-connected-foreground = "\"${color}\"";
          format-connected-background = "\"${foreground}\"";
          label-connected = "\"%signal%%\"";

          format-disconnected = "\"<label-disconnected> %{T2}󰤠{T-} \"";
          format-disconnected-foreground = "\"${color}\"";
          format-disconnected-background = "\"${foreground}\"";
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
          format-1-foreground = "\"${foreground}\"";
          format-1-background = "\"${color}\"";
          # on
          hook-2 = "\"\"";
          format-2 = "\"%{T2}%{T-} \"";
          format-2-foreground = "\"${foreground}\"";
          format-2-background = "\"${color}\"";

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
          format-1-background = "\"${foreground}\"";
          format-1-foreground = "\"${color}\"";
          hook-1 = "\"${pamixer} --get-volume\"";

          # low volume
          format-2 = "\"<label> %{T2}󰕾%{T-} \"";
          format-2-background = "\"${foreground}\"";
          format-2-foreground = "\"${color}\"";
          hook-2 = "\"${pamixer} --get-volume\"";

        };
        "module/date" = {
          type = "\"internal/date\"";
          interval = "\"1.0\"";
          date = "\"%a, %d %b %Y %{T2}󰃭%{T-}\"";

          # REVIEW: not working
          # click-left = "\"${pkgs.yad}/bin/yad --calendar --undecorated --fixed --close-on-unfocus --no-buttons &\"";

          label = "\"%date% \"";
          label-background = "\"${color}\"";
          label-foreground = "\"${foreground}\"";
          label-padding-right = "\"1\"";

        };
        "module/time" = {
          type = "\"internal/date\"";
          interval = "\"1.0\"";
          time = "\"%H:%M:%S %{T2}󰥔%{T-}\"";

          label = "\"%time% \"";
          format-background = "\"${foreground}\"";
          format-foreground = "\"${color}\"";
        };
        "module/even-l" = {
          type = "\"custom/text\"";
          label="\"%{O-1}%{O1}%{B${color}}%{T3} %{T-}%{B-}%{O-25}%{T3}%{T-}\"";
          label-foreground="\"${foreground}\"";
        };
        "module/even-r" = {
          type = "\"custom/text\"";
          label-background = "\"${color}\"";
          label-foreground = "\"${foreground}\"";
          label = "\"%{T3}%{T-}%{O-1}\"";
        };
        "module/odd-r" = {
          type = "\"custom/text\"";
          label-background = "\"${foreground}\"";
          label-foreground = "\"${color}\"";
          label = "\"%{T3}%{T-}%{O-1}\"";
        };
        "module/left" = {
          type = "\"custom/text\"";
          label = "\"%{F${color}}%{T3}%{T-}%{F-}%{O-12}%{F${background}}%{T3}%{T-}%{F-}%{O-12}%{F${color}}%{T3}%{T-}%{F-}\"";
          format = "\"<label>%{O-2}\"";
        };
        "module/right" = {
          type = "\"custom/text\"";
          label = "\"%{O23}%{F${color}}%{T3}%{T-}%{F-}%{F-}%{O-36}%{F${background}}%{T3}%{T-}%{F-}%{O-36}%{F${color}}%{T3}%{T-}\"";
          format = "\"<label>%{O-2}\"";
        };
      };
    };
  };
}

