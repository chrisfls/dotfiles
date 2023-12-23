{ config, lib, pkgs, specialArgs, ... }:
let
  cfg = config.module.polybar;

  colors = config.module.themes.color-scheme;

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

  sxhkd-mode =
    let
      script = pkgs.writeShellScriptBin "sxhkd-mode"
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
    "${script}/bin/sxhkd-mode";

  black = colors.background;
  white = colors.foreground;
  primary = colors.black;
  accent = colors.blueBright;
in
{
  options.module.polybar.enable = lib.mkEnableOption "Enable polybar module";

  config = lib.mkIf cfg.enable {
    # HACK: disable polybar service
    systemd.user.services.polybar = lib.mkForce { };

    xsession.windowManager.bspwm.startupPrograms = [
      "systemd-cat -t polybar systemd-run --user --scope --property=OOMPolicy=continue -u polybar ${pkgs.polybar}/bin/polybar"
      "systemd-cat -t sxhkd-mode systemd-run --user --scope --property=OOMPolicy=continue -u sxhkd-mode ${sxhkd-mode}"
    ];

    services.polybar = {
      enable = true;
      script = "";
      settings = {
        settings.screenchange-reload = "\"true\"";
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
          # dim-value = "\"0.5\"";

          # styling

          # base font
          font-0 = "\"CaskaydiaCoveNerdFontMono:size=10;4\"";

          # icon font
          font-1 = "\"CaskaydiaCoveNerdFontPropo:size=11;3\"";

          # powerline font
          font-2 = "\"CaskaydiaCoveNerdFont:size=20;6\"";

          background = "\"${black}\"";
          foreground = "\"${white}\"";

          # modules
          modules-left = "\"menu menu-l mode keyboard even-l workspaces toggle tray right\"";
          modules-center = "\"left title right \"";
          modules-right = "\"left filesystem even-r temperature odd-r memory even-r cpu odd-r wired even-r wireless odd-r bluetooth even-r audio odd-r date even-r time menu-r session\"";
          format-radius = "\"32.0\"";
        };
        "module/menu" = {
          type = "\"custom/text\"";

          click-left = "\"rofi -show drun -theme \"${config.xdg.configHome}/rofi/launchers/type-3/style-1.rasi\"\"";

          format = "\" %{T2}󰺮%{T-} Apps\"";
          format-background = "\"${accent}\"";
          format-foreground = "\"${white}\"";
        };
        "module/session" = {
          type = "\"custom/text\"";

          click-left = "\"${config.xdg.configHome}/rofi/powermenu/type-1/powermenu.sh\"";

          format = "\"${config.home.username} %{T2}󰍃%{T-} \"";
          format-background = "\"${accent}\"";
          format-foreground = "\"${white}\"";
        };
        "module/mode" = {
          type = "\"custom/ipc\"";
          # startup
          initial = "\"1\"";
          # move
          hook-0 = "\"\"";
          format-0 = "\" %{O-1}%{T2}󰆾%{T-}\"";
          format-0-background = "\"${white}\"";
          format-0-foreground = "\"${primary}\"";
          format-0-padding = "\"0\"";
          # resize
          hook-1 = "\"\"";
          format-1 = "\" %{O1}%{T2}󰁌%{T-}\"";
          format-1-background = "\"${white}\"";
          format-1-foreground = "\"${primary}\"";
          format-1-padding = "\"0\"";
        };
        "module/keyboard" = {
          type = "\"internal/xkeyboard\"";

          # REVIEW: not working
          # click-left = "\"${pkgs.numlockx}/bin/numlockx toggle &\"";
          blacklist-0 = "\"caps lock\"";
          blacklist-1 = "\"scroll lock\"";

          format-foreground = "\"${primary}\"";
          format-background = "\"${white}\"";

          # label-indicator-off-capslock = "\" %{T2}󰯫%{T-}\"";
          # label-indicator-off-capslock-foreground = "\"${primary}\"";
          # label-indicator-on-capslock = "\" %{T2}󰬈%{T-}\"";
          # label-indicator-on-capslock-foreground = "\"${primary}\"";

          label-indicator-off-numlock = "\"%{T2}󰰒%{T-}\"";
          label-indicator-off-numlock-foreground = "\"${primary}\"";
          label-indicator-on-numlock = "\"%{T2}󰬕%{T-}\"";
          label-indicator-on-numlock-foreground = "\"${primary}\"";

          # label-indicator-off-scrolllock = "\" %{T2}󰰡%{T-}\"";
          # label-indicator-off-scrolllock-foreground = "\"${primary}\"";
          # label-indicator-on-scrolllock = "\" %{T2}󰬚%{T-}\"";
          # label-indicator-on-scrolllock-foreground = "\"${primary}\"";
          label-layout = "\"\"";
          # label-layout-padding = "\"0\"";
        };
        "module/mode-keyboard-workspaces" = {
          type = "\"custom/text\"";
          label-foreground = "\"${primary}\"";
          label = "\"%{B${primary}} %{B-}%{O-25}%{T3}%{T-}\"";
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
          label-active = "";
          label-active-background = "\"${primary}\"";
          label-active-foreground = "\"${white}\"";
          label-active-padding = "\"1\"";

          label-empty = "\"\"";
          label-empty-background = "\"${primary}\"";
          label-empty-foreground = "\"${black}\"";
          label-empty-padding = "\"1\"";

          label-occupied = "\"\"";
          label-occupied-background = "\"${primary}\"";
          label-occupied-foreground = "\"${black}\"";
          label-occupied-padding = "\"1\"";

          label-urgent = "\"\"";
          label-urgent-background = "\"${primary}\"";
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
          format-0-foreground = "\"${black}\"";
          format-0-background = "\"${primary}\"";
          format-0-padding = "\"1\"";
          hook-1 = "\"\"";
          format-1 = "\"%{T2}%{T-}\"";
          format-1-foreground = "\"${white}\"";
          format-1-background = "\"${primary}\"";
          format-1-padding = "\"1\"";
        };
        "module/tray" = {
          type = "\"internal/tray\"";
          tray-spacing = "\"1pt\"";
          tray-padding = "\"1pt\"";
          format = "\"%{O-2}<tray>\"";
          format-background = "\"${primary}\"";
          tray-foreground = "\"${white}\"";
          tray-background = "\"${primary}\"";
          tray-size = "\"100%\"";
        };
        "module/title" = {
          type = "\"internal/xwindow\"";
          label = "\"%title:0:64:...%\"";
          label-background = "\"${primary}\"";
          label-empty = "\"Desktop\"";
          label-empty-background = "\"${primary}\"";
        };
        "module/filesystem" = {
          type = "\"internal/fs\"";
          warn-percentage = "\"50\"";
          mount-0 = "\"/\"";

          format-mounted = "\"<label-mounted> %{T2}󰋊%{T-} \"";
          format-mounted-foreground = "\"${white}\"";
          format-mounted-background = "\"${primary}\"";
          label-mounted = "\"%free%\"";

          format-warn = "\"<label-warn>\"";
          format-warn-foreground = "\"${white}\"";
          format-warn-background = "\"${primary}\"";
          label-warn = "\"%free%\"";

          format-unmounted = "\"<label-unmounted> %{T2}󰋊%{T-} \"";
          format-unmounted-foreground = "\"${white}\"";
          format-unmounted-background = "\"${primary}\"";
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
          format-foreground = "\"${primary}\"";
          format-background = "\"${white}\"";
          label = "\"%temperature-c%\"";

          format-warn = "\"<label-warn> %{T2}<ramp>%{T-} \"";
          format-warn-foreground = "\"${primary}\"";
          format-warn-background = "\"${white}\"";
          label-warn = "\"%temperature-c%\"";
        };
        "module/memory" = {
          type = "\"internal/memory\"";

          format = "\"<label> %{T2}%{T-} \"";
          format-foreground = "\"${white}\"";
          format-background = "\"${primary}\"";
          label = "\"%percentage_used%%\"";

          format-warn = "\"<label> %{T2}%{T-} \"";
          format-warn-foreground = "\"${white}\"";
          format-warn-background = "\"${primary}\"";
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
          format-foreground = "\"${primary}\"";
          format-background = "\"${white}\"";

          format-warn = "\"%{F${colors.red}}<ramp-load>%{F${colors.red}} %{T2}%{T-} \"";
          format-warn-foreground = "\"${primary}\"";
          format-warn-background = "\"${white}\"";
        };
        "module/wired" = {
          type = "\"internal/network\"";
          interface-type = "\"wired\"";
          interval = "\"10\"";

          format-connected = "\"<label-connected>\"";
          format-connected-foreground = "\"${white}\"";
          format-connected-background = "\"${primary}\"";
          label-connected = "\"%netspeed% %{T2}󰈁%{T-} \"";

          format-disconnected = "\"<label-disconnected>\"";
          format-disconnected-foreground = "\"${white}\"";
          format-disconnected-background = "\"${primary}\"";
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
          format-connected-foreground = "\"${primary}\"";
          format-connected-background = "\"${white}\"";
          label-connected = "\"%signal%%\"";

          format-disconnected = "\"<label-disconnected> %{T2}󰤠{T-} \"";
          format-disconnected-foreground = "\"${primary}\"";
          format-disconnected-background = "\"${white}\"";
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
          format-1-foreground = "\"${white}\"";
          format-1-background = "\"${primary}\"";
          # on
          hook-2 = "\"\"";
          format-2 = "\"%{T2}%{T-} \"";
          format-2-foreground = "\"${white}\"";
          format-2-background = "\"${primary}\"";

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
          format-1-background = "\"${white}\"";
          format-1-foreground = "\"${primary}\"";
          hook-1 = "\"${pamixer} --get-volume\"";

          # low volume
          format-2 = "\"<label> %{T2}󰕾%{T-} \"";
          format-2-background = "\"${white}\"";
          format-2-foreground = "\"${primary}\"";
          hook-2 = "\"${pamixer} --get-volume\"";

        };
        "module/date" = {
          type = "\"internal/date\"";
          interval = "\"1.0\"";
          date = "\"%a, %d %b %Y %{T2}󰃭%{T-}\"";

          # REVIEW: not working
          # click-left = "\"${pkgs.yad}/bin/yad --calendar --undecorated --fixed --close-on-unfocus --no-buttons &\"";

          label = "\"%date% \"";
          label-background = "\"${primary}\"";
          label-foreground = "\"${white}\"";
          label-padding-right = "\"1\"";

        };
        "module/time" = {
          type = "\"internal/date\"";
          interval = "\"1.0\"";
          time = "\"%H:%M:%S %{T2}󰥔%{T-}\"";

          label = "\"%time% \"";
          format-background = "\"${white}\"";
          format-foreground = "\"${primary}\"";
        };
        "module/menu-r" = {
          type = "\"custom/text\"";
          label-background = "\"${white}\"";
          label-foreground = "\"${accent}\"";
          label = "\"%{T3}%{T-}%{O-1}\"";
        };
        "module/menu-l" = {
          type = "\"custom/text\"";
          label = "\"%{O-1}%{O1}%{B${white}}%{T3} %{T-}%{B-}%{O-25}%{T3}%{T-}\"";
          label-foreground = "\"${accent}\"";
        };
        "module/left" = {
          type = "\"custom/text\"";
          label = "\"%{F${primary}}%{T3}%{T-}%{F-}%{O-12}%{F${black}}%{T3}%{T-}%{F-}%{O-12}%{F${primary}}%{T3}%{T-}%{F-}\"";
          format = "\"<label>%{O-2}\"";
        };
        "module/right" = {
          type = "\"custom/text\"";
          label = "\"%{O23}%{F${primary}}%{T3}%{T-}%{F-}%{F-}%{O-36}%{F${black}}%{T3}%{T-}%{F-}%{O-36}%{F${primary}}%{T3}%{T-}\"";
          format = "\"<label>%{O-2}\"";
        };
        "module/even-l" = {
          type = "\"custom/text\"";
          label = "\"%{O-1}%{O1}%{B${primary}}%{T3} %{T-}%{B-}%{O-25}%{T3}%{T-}\"";
          label-foreground = "\"${white}\"";
        };
        "module/even-r" = {
          type = "\"custom/text\"";
          label-background = "\"${primary}\"";
          label-foreground = "\"${white}\"";
          label = "\"%{T3}%{T-}%{O-1}\"";
        };
        "module/odd-r" = {
          type = "\"custom/text\"";
          label-background = "\"${white}\"";
          label-foreground = "\"${primary}\"";
          label = "\"%{T3}%{T-}%{O-1}\"";
        };
      };
    };
  };
}

