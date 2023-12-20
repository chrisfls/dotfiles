{ config, lib, pkgs, specialArgs, ... }:
let
  cfg = config.extra.polybar;

  polybar-msg = "${pkgs.polybar}/bin/polybar-msg";

  sxhkd-chord-mode =
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
    "";

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

  bluedevil = "${pkgs.libsForQt5.bluedevil}/bin/bluedevil";

  yad = "${pkgs.yad}/bin/yad";

  pavucontrol-qt = "${pkgs.pavucontrol-qt}/bin/pavucontrol-qt";
in
{
  options.extra.polybar = {
    enable = lib.mkEnableOption "Enable polybar module";
    color-scheme = lib.mkOption {
      type = lib.types.attrs;
      default = specialArgs.color-schemes.popping-and-locking-black;
    };
  };

  ##

  config = lib.mkIf cfg.enable {
    systemd.user.services.sxhkd-chord-mode = {
      Unit.Description = "Trigger sxhkd chord indicator";
      Service.ExecStart = sxhkd-chord-mode;
    };

    services.polybar = {
      enable = true;
      script = "polybar -r &";
      settings = {
        colors = {
          background = color-scheme.background;
          foreground = color-scheme.foreground;
          blackBright = color-scheme.blackBright;
          whiteBright = color-scheme.whiteBright;
          redBright = color-scheme.redBright;
          yellowBright = color-scheme.yellowBright;
          greenBright = color-scheme.greenBright;
          cyanBright = color-scheme.cyanBright;
          blueBright = color-scheme.blueBright;
          magentaBright = color-scheme.magentaBright;
          black = color-scheme.black;
          white = color-scheme.white;
          red = color-scheme.red;
          yellow = color-scheme.yellow;
          green = color-scheme.green;
          cyan = color-scheme.cyan;
          blue = color-scheme.blue;
          magenta = color-scheme.magenta;
        };
        "bar/topbar" = {
          enable-ipc = true;

          dpi = 144;
          height = "18pt";
          width = "100%";
          line-size = "2pt";

          padding-left = 1;
          padding-right = 1;
          border-bottom-size = 4;
          separator = " ";

          font-0 = "CaskaydiaCoveNerdFontMono:size=10;6";
          font-1 = "CaskaydiaCoveNerdFontPropo:size=11;5";

          background = "$${colors.background}";
          foreground = "$${colors.foreground}";

          # modules
          modules-left = "workspaces mode keyboard toggle tray";
          modules-center = "title";
          modules-right = "filesystem temperature memory cpu wired wireless bluetooth audio date time";
        };
        # left
        "module/workspaces" = {
          type = "internal/bspwm";

          pin-workspaces = true;
          enable-scroll = false;
          reverse-scroll = false;
          fuzzy-match = false;
          occupied-scroll = false;

          label-active = "%name%";
          label-active-padding = 1;
          label-empty = "%name%";
          label-empty-padding = 1;
          label-occupied = "%name%";
          label-occupied-padding = 1;
          label-urgent = "%name%";
          label-urgent-padding = 1;

          label-active-overline = "$${colors.foreground}";
          label-empty-foreground = "$${colors.black}";
          label-occupied-foreground = "$${colors.blackBright}";
          label-urgent-background = "$${colors.redBright}";
        };
        "module/mode" = {
          type = "custom/ipc";
          initial = 1;

          hook-0 = "";
          format-0 = "%{T3}󰆾%{T-}";
          format-0-foreground = "$${colors.blackBright}";

          hook-1 = "";
          format-1 = "%{T3}󰩨%{T-}";
          format-1-foreground = "$${colors.foreground}";
        };
        "module/keyboard" = {
          type = "internal/xkeyboard";

          click-left = "exec ${pkgs.numlockx}/bin/numlockx toggle";

          blacklist-0 = "caps lock";
          blacklist-1 = "scroll lock";

          label-layout = "";
          format-foreground = "$${colors.foreground}";

          label-indicator-off-capslock = "%{T3}󰯫%{T-} ";
          label-indicator-off-capslock-foreground = "$${colors.blackBright}";
          label-indicator-on-capslock = "%{T3}󰬈%{T-} ";
          label-indicator-on-capslock-foreground = "$${colors.foreground}";

          label-indicator-off-numlock = "%{T3}󰰒%{T-} ";
          label-indicator-off-numlock-foreground = "$${colors.blackBright}";
          label-indicator-on-numlock = "%{T3}󰬕%{T-} ";
          label-indicator-on-numlock-foreground = "$${colors.foreground}";

          label-indicator-off-scrolllock = "%{T3}󰰡%{T-} ";
          label-indicator-off-scrolllock-foreground = "$${colors.blackBright}";
          label-indicator-on-scrolllock = "%{T3}󰬚%{T-} ";
          label-indicator-on-scrolllock-foreground = "$${colors.foreground}";
        };
        "module/toggle" = {
          type = "custom/ipc";
          initial = 1;

          scroll-down = "${toggle} toggle tray &";
          scroll-up = "${toggle} toggle tray &";

          hook-0 = "";
          format-0 = "%{T3}%{T-}";
          format-0-foreground = "$${colors.foreground}";
          format-0-padding = "0.5";

          hook-1 = "";
          format-1 = "%{T3}%{T-}";
          format-1-foreground = "$${colors.foreground}";
          format-1-padding = "0.5";
        };
        "module/tray" = {
          type = "internal/tray";
          tray-size = "100%";
          tray-foreground = "$${colors.foreground}";
          tray-padding = "1pt";
          tray-spacing = "1pt";
        };
        # middle
        "module/title" = {
          type = "internal/xwindow";
          label = " %title:0:48:...% ";
        };
        # right
        "module/filesystem" = {
          type = internal/fs;
          warn-percentage = 50;
          mount-0 = "/";

          format-mounted-foreground = "$${colors.magenta}";
          format-mounted-overline = "$${colors.magenta}";
          label-mounted = " %free% %{T3}󰋊%{T-} ";

          format-warn = "<label-warn>";
          format-warn-foreground = "$${colors.magentaBright}";
          format-warn-overline = "$${colors.magentaBright}";
          label-warn = " %free% e %{T3}󰋊%{T-} ";

          format-unmounted-foreground = "$${colors.magenta}";
          format-unmounted-overline = "$${colors.magenta}";
          label-unmounted = " ? %{T3}󰋊%{T-} ";
        };
        "module/temperature" = {
          type = "internal/temperature";
          interval = "1";
          thermal-zone = "0";
          zone-type = "x86_pkg_temp";
          base-temperature = "20";
          warn-temperature = "80";
          ramp-0 = "";
          ramp-1 = "";
          ramp-2 = "";
          ramp-3 = "";
          ramp-4 = "";

          format = "<label> %{T3}<ramp>%{T-} ";
          format-foreground = "$${colors.red}";
          format-overline = "$${colors.red}";
          label = " %temperature-c%";

          format-warn = "<label-warn> %{T3}<ramp>%{T-} ";
          format-warn-foreground = "$${colors.redBright}";
          format-warn-overline = "$${colors.redBright}";
          label-warn = " %temperature-c%";
        };
        "module/memory" = {
          type = "internal/memory";
          interval = "0.5";

          format = "<label>";
          format-foreground = "$${colors.yellow}";
          format-overline = "$${colors.yellow}";
          label = " %percentage_used%% %{T3}%{T-} ";

          format-warn = "<label>";
          format-warn-foreground = "$${colors.yellowBright}";
          format-warn-overline = "$${colors.yellowBright}";
          label-warn = " %percentage_used%% %{T3}%{T-} ";
        };
        "module/cpu" = {
          type = "internal/cpu";
          interval = "0.25";

          ramp-load-0 = "▁";
          ramp-load-1 = "▂";
          ramp-load-2 = "▃";
          ramp-load-3 = "▄";
          ramp-load-4 = "▅";
          ramp-load-5 = "▆";
          ramp-load-6 = "▇";
          ramp-load-7 = "█";
          warn-percentage = "50";

          format = "<label-warn> <ramp-load> %{T3}%{T-} ";
          format-foreground = "$${colors.green}";
          format-overline = "$${colors.green}";
          label = "";

          format-warn = "<label-warn> <ramp-load> %{T3}%{T-} ";
          format-warn-foreground = "$${colors.greenBright}";
          format-warn-overline = "$${colors.greenBright}";
          label-warn = "";
        };
        "module/wired" = {
          type = "internal/network";
          interface-type = "wired";
          interval = "1";

          format-connected = "<label-connected>";
          format-connected-foreground = "$${colors.cyanBright}";
          format-connected-overline = "$${colors.cyanBright}";
          label-connected = " %netspeed% %{T3}󰈁%{T-} ";

          format-disconnected = "<label-disconnected>";
          format-disconnected-foreground = "$${colors.cyan}";
          format-disconnected-overline = "$${colors.cyan}";
          label-disconnected = " down  %{T3}󰈂{T-} ";
        };
        "module/wireless" = {
          type = "internal/network";
          interface-type = "wireless";

          ramp-signal-0 = "󰤯";
          ramp-signal-1 = "󰤟";
          ramp-signal-2 = "󰤢";
          ramp-signal-3 = "󰤥";
          ramp-signal-4 = "󰤨";

          format-connected = "<label-connected> %{T3}<ramp-signal>%{T-} ";
          format-connected-foreground = "$${colors.cyanBright}";
          format-connected-overline = "$${colors.cyanBright}";
          label-connected = " %signal%%";

          format-disconnected = "<label-disconnected> %{T3}󰤠{T-} ";
          format-disconnected-foreground = "$${colors.cyan}";
          format-disconnected-overline = "$${colors.cyan}";
          label-disconnected = " off";
        };
        "module/bluetooth" = {
          # TODO: install bluez on user level https://bbs.archlinux.org/viewtopic.php?id=215080
          type = "custom/ipc";
          initial = 1;

          click-left = "${bluetooth} --toggle &";
          click-right = "${bluedevil}";

          hook-0 = "${bluetooth} &";
          label = " %output% ";

          # off state
          format-1 = " %{T3}󰂲%{T-} ";
          format-1-foreground = "$${colors.blue}";
          format-1-overline = "$${colors.blue}";
          hook-1 = "";

          # on state
          format-2 = " %{T3}%{T-} ";
          format-2-foreground = "$${colors.blueBright}";
          format-2-overline = "$${colors.blueBright}";
          hook-2 = "";
        };
        # REVIEW: https://github.com/polybar/polybar-scripts/blob/master/polybar-scripts/system-usb-udev/system-usb-udev.sh
        "module/audio" = {
          type = "custom/ipc";
          initial = 1;

          click-left = "${pipewire} --mute &";
          click-right = "${pavucontrol-qt} &";
          scroll-down = "${pipewire} --down &";
          scroll-up = "${pipewire} --up &";

          hook-0 = "${pipewire} &";
          label = " %output%%";

          # mute state
          format-1 = "<label> %{T3}󰝟%{T-} ";
          format-1-foreground = "$${colors.blue}";
          format-1-overline = "$${colors.blue}";
          hook-1 = "${pamixer} --get-volume";

          # on state
          format-2 = "<label> %{T3}󰕾%{T-} ";
          format-2-foreground = "$${colors.blueBright}";
          format-2-overline = "$${colors.blueBright}";
          hook-2 = "${pamixer} --get-volume";
        };
        "module/date" = {
          type = "internal/date";
          interval = "1.0";

          click-left = "${yad} --calendar --undecorated --fixed --close-on-unfocus --no-buttons";

          date = "%a, %d %b %Y %{T3}󰃭%{T-}";
          label = " %date% ";
          label-foreground = "$${colors.foreground}";
          label-overline = "$${colors.foreground}";
        };
        "module/time" = {
          type = "internal/date";
          interval = "1.0";

          label = " %time% ";
          label-overline = "$${colors.foreground}";
          time = "%H:%M:%S %{T3}󰥔%{T-}";
        };
      };
    };
  };
}

