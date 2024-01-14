# TODO: https://github.com/polybar/polybar-scripts/blob/master/polybar-scripts/system-usb-udev/system-usb-udev.sh
{ config, lib, pkgs, specialArgs, ... }:
let
  inherit (config.modules.polybar) enable;
  inherit (config.modules.themes.color-scheme) background foreground black redBright blueBright;

  polybar-msg = "${config.services.polybar.package}/bin/polybar-msg";
  bluetoothctl = "${pkgs.bluez}/bin/bluetoothctl";
  pamixer = "${pkgs.pamixer}/bin/pamixer";
  dunstctl = "${pkgs.dunst}/bin/dunstctl";

  script = name: "\"$SCRIPT/${name}\"";
  rofi-menu = script "rofi-menu";
  # rofi-power-menu = script "rofi-power-menu";
  toggle = script "toggle";
  bluetooth = script "bluetooth";
  pipewire = script "pipewire";
  dunst-toggle = script "dunst-toggle";
in
{
  options.modules.polybar.enable = lib.mkEnableOption "Enable polybar module";

  config = lib.mkIf enable {
    pacman.pkgs.bluez = [ "extra/bluez" "extra/bluez-utils" ];

    modules.script.install = {
      toggle =
        ''
          fst=$1
          shift

          for arg in "$@"; do
            ${polybar-msg} action "#$arg.module_toggle"
          done

          ${polybar-msg} action $fst next
        '';
      bluetooth =
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

      pipewire =
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

      dunst-toggle =
        ''
          ${dunstctl} set-paused toggle
          ${polybar-msg} action notifications next
        '';
    };

    systemd.user.services.polybar = {
      # i3wm fix
      Unit.After = lib.mkIf (config.modules.i3wm.enable) [ "graphical-session-i3.target" ];
      Install.WantedBy = lib.mkIf (config.modules.i3wm.enable) (lib.mkForce [ "graphical-session-i3.target" ]);

      # home manager fix
      Service.Environment = lib.mkIf (config.presets.non-nixos) (lib.mkForce [ ]);
    };

    # i3wm launch
    xsession.windowManager.i3.config.startup = lib.mkIf (config.modules.i3wm.enable) [
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

          background = "\"${background}\"";
          foreground = "\"${foreground}\"";

          # modules
          modules-left = "\"workspaces even-l notifications menu toggle tray right title\""; # menu menu-l 
          modules-center = "\"\""; # left title right 
          modules-right = "\"left filesystem even-r temperature odd-r memory even-r cpu odd-r wired even-r wireless odd-r bluetooth even-r audio odd-r date even-r time\""; #  menu-r session

          format-radius = "\"32.0\"";
        };
        /*
        "module/menu" = {
          type = "\"custom/ipc\"";

          click-left = "\"${rofi-menu}\"";

          # startup
          initial = "\"1\"";
          # move
          hook-0 = "\"\"";
          format-0 = "\" %{T2}󰺮%{T-} Applications\"";
          format-0-background = "\"${background}\"";
          format-0-foreground = "\"${foreground}\"";
          format-0-padding = "\"0\"";
          # resize
          hook-1 = "\"\"";
          format-1 = "\" %{T2}󰺮%{T-} Applications\"";
          format-1-background = "\"${background}\"";
          format-1-foreground = "\"${red}\"";
          format-1-padding = "\"0\"";
          # launcher
          hook-2 = "\"\"";
          format-2 = "\" %{T2}󰺮%{T-} Applications\"";
          format-2-background = "\"${background}\"";
          format-2-foreground = "\"${blue}\"";
          format-2-padding = "\"0\"";
        };
        */
        "module/menu" = {
          type = "\"custom/ipc\"";

          click-left = "\"${rofi-menu}\"";

          # startup
          initial = "\"1\"";
          # move
          hook-0 = "\"\"";
          format-0 = "\"%{T2}%{T-}%{O1}\"";
          format-0-background = "\"${black}\"";
          format-0-foreground = "\"${foreground}\"";
          format-0-padding = "\"0\"";
          # resize
          hook-1 = "\"\"";
          format-1 = "\"%{T2}%{T-}%{O1}\"";
          format-1-background = "\"${black}\"";
          format-1-foreground = "\"${redBright}\"";
          format-1-padding = "\"0\"";
          # launcher
          hook-2 = "\"\"";
          format-2 = "\"%{T2}%{T-}%{O1}\"";
          format-2-background = "\"${black}\"";
          format-2-foreground = "\"${blueBright}\"";
          format-2-padding = "\"0\"";
        };
        /*
        "module/session" = {
          type = "\"custom/text\"";

          click-left = "\"${rofi-power-menu}\"";

          format = "\"${config.home.username} %{T2}󰍃%{T-} \"";
          format-background = "\"${background}\"";
          format-foreground = "\"${foreground}\"";
        };
        */
        "module/toggle" = {
          type = "\"custom/ipc\"";
          click-left = "\"${toggle} toggle tray &\"";
          scroll-down = "\"${toggle} toggle tray &\"";
          scroll-up = "\"${toggle} toggle tray &\"";
          initial = "\"1\"";
          hook-0 = "\"\"";
          format-0 = "\"%{T2}%{T-}\"";
          format-0-foreground = "\"${foreground}\"";
          format-0-background = "\"${black}\"";
          format-0-padding = "\"1\"";
          hook-1 = "\"\"";
          format-1 = "\"%{T2}%{T-}\"";
          format-1-foreground = "\"${background}\"";
          format-1-background = "\"${black}\"";
          format-1-padding = "\"1\"";
        };
        "module/notifications" = {
          type = "\"custom/ipc\"";
          hidden = "\"false\"";
          click-left = "\"${dunst-toggle} &\"";
          click-right = "\"rofi-dunst &\"";
          scroll-down = "\"${dunstctl} history-pop &\"";
          scroll-up = "\"${dunstctl} close &\"";
          initial = "\"1\"";
          hook-0 = "\"\"";
          format-0 = "\"%{T2}%{T-}\"";
          format-0-foreground = "\"${foreground}\"";
          format-0-background = "\"${black}\"";
          format-0-padding = "\"1\"";
          hook-1 = "\"\"";
          format-1 = "\"%{T2}%{T-}\"";
          format-1-foreground = "\"${foreground}\"";

          format-1-background = "\"${black}\"";
          format-1-padding = "\"1\"";
        };
        "module/tray" = {
          type = "\"internal/tray\"";
          hidden = "\"false\"";
          tray-spacing = "\"1pt\"";
          tray-padding = "\"1pt\"";
          format = "\"%{O-4}<tray>\"";
          format-background = "\"${black}\"";
          tray-foreground = "\"${foreground}\"";
          tray-background = "\"${black}\"";
          tray-size = "\"75%\"";
        };
        /*
        "module/title" = {
          type = "\"internal/xwindow\"";
          label = "\"%title:0:64:...%\"";
          label-background = "\"${black}\"";
          label-empty = "\"Desktop\"";
          label-empty-background = "\"${black}\"";
        };
        */
        "module/title" = {
          type = "\"internal/xwindow\"";
          label = "\"%title:0:100:...%\"";
          label-background = "\"${background}\"";
          label-empty = "\"Desktop\"";
          label-empty-background = "\"${background}\"";
          format-padding = "\"3\"";
        };
        "module/filesystem" = {
          type = "\"internal/fs\"";
          warn-percentage = "\"50\"";
          mount-0 = "\"/\"";

          format-mounted = "\"<label-mounted> %{T2}󰋊%{T-} \"";
          format-mounted-foreground = "\"${foreground}\"";
          format-mounted-background = "\"${black}\"";
          label-mounted = "\"%free%\"";

          format-warn = "\"<label-warn>\"";
          format-warn-foreground = "\"${foreground}\"";
          format-warn-background = "\"${black}\"";
          label-warn = "\"%free%\"";

          format-unmounted = "\"<label-unmounted> %{T2}󰋊%{T-} \"";
          format-unmounted-foreground = "\"${foreground}\"";
          format-unmounted-background = "\"${black}\"";
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
          format-foreground = "\"${black}\"";
          format-background = "\"${foreground}\"";
          label = "\"%temperature-c%\"";

          format-warn = "\"<label-warn> %{T2}<ramp>%{T-} \"";
          format-warn-foreground = "\"${black}\"";
          format-warn-background = "\"${foreground}\"";
          label-warn = "\"%temperature-c%\"";
        };
        "module/memory" = {
          type = "\"internal/memory\"";

          format = "\"<label> %{T2}%{T-} \"";
          format-foreground = "\"${foreground}\"";
          format-background = "\"${black}\"";
          label = "\"%percentage_used%%\"";

          format-warn = "\"<label-warn> %{T2}%{T-} \"";
          format-warn-foreground = "\"${foreground}\"";
          format-warn-background = "\"${black}\"";
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
          format-foreground = "\"${black}\"";
          format-background = "\"${foreground}\"";

          format-warn = "\"%{F${redBright}}<ramp-load>%{F${redBright}} %{T2}%{T-} \"";
          format-warn-foreground = "\"${black}\"";
          format-warn-background = "\"${foreground}\"";
        };
        "module/wired" = {
          type = "\"internal/network\"";
          interface-type = "\"wired\"";
          interval = "\"10\"";

          format-connected = "\"<label-connected>\"";
          format-connected-foreground = "\"${foreground}\"";
          format-connected-background = "\"${black}\"";
          label-connected = "\"%netspeed% %{T2}󰈁%{T-} \"";

          format-disconnected = "\"<label-disconnected>\"";
          format-disconnected-foreground = "\"${foreground}\"";
          format-disconnected-background = "\"${black}\"";
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
          format-connected-foreground = "\"${black}\"";
          format-connected-background = "\"${foreground}\"";
          label-connected = "\"%signal%%\"";

          format-disconnected = "\"<label-disconnected> %{T2}󰤠{T-} \"";
          format-disconnected-foreground = "\"${black}\"";
          format-disconnected-background = "\"${foreground}\"";
          label-disconnected = "\"off\"";
        };
        "module/bluetooth" = {
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
          format-1-background = "\"${black}\"";
          # on
          hook-2 = "\"\"";
          format-2 = "\"%{T2}%{T-} \"";
          format-2-foreground = "\"${foreground}\"";
          format-2-background = "\"${black}\"";
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
          format-1-foreground = "\"${black}\"";
          hook-1 = "\"${pamixer} --get-volume\"";

          # low volume
          format-2 = "\"<label> %{T2}󰕾%{T-} \"";
          format-2-background = "\"${foreground}\"";
          format-2-foreground = "\"${black}\"";
          hook-2 = "\"${pamixer} --get-volume\"";

        };
        # TODO: replace with custom module to support calendar
        /*
        "module/date" = {
          type = "\"custom/script\"";
          interval = "\"1.0\"";
          # posx: 3264 + 1280
          click-left = "\"${pkgs.yad}/bin/yad --calendar --undecorated --fixed --close-on-unfocus --no-buttons --posx=4544 --posy=36 &\"";
          exec = "\"echo \"$(${pkgs.toybox}/bin/date +'%a, %d %b %Y') %{T2}󰃭%{T-}\"\"";

          label = "\"%output% \"";
          label-background = "\"${black}\"";
          label-foreground = "\"${foreground}\"";
          label-padding-right = "\"1\"";
        };
        */
        "module/date" = {
          type = "\"internal/date\"";
          interval = "\"1.0\"";
          time = "\"%a, %d %b %Y %{T2}󰃭%{T-}\"";

          label = "\"%time% \"";
          format-background = "\"${black}\"";
          format-foreground = "\"${foreground}\"";
        };
        "module/time" = {
          type = "\"internal/date\"";
          interval = "\"1.0\"";
          time = "\"%H:%M:%S %{T2}󰥔%{T-}\"";

          label = "\"%time% \"";
          format-background = "\"${foreground}\"";
          format-foreground = "\"${black}\"";
        };
        "module/menu-r" = {
          type = "\"custom/text\"";
          label-background = "\"${foreground}\"";
          label-foreground = "\"${background}\"";
          label = "\"%{T3}%{T-}%{O-1}\"";
        };
        "module/menu-l" = {
          type = "\"custom/text\"";
          label = "\"%{O-1}%{O1}%{B${foreground}}%{T3} %{T-}%{B-}%{O-25}%{T3}%{T-}\"";
          label-foreground = "\"${background}\"";
        };
        "module/left" = {
          type = "\"custom/text\"";
          label = "\"%{F${black}}%{T3}%{T-}%{F-}%{O-12}%{F${background}}%{T3}%{T-}%{F-}%{O-12}%{F${black}}%{T3}%{T-}%{F-}\"";
          format = "\"<label>%{O-2}\"";
        };
        "module/right" = {
          type = "\"custom/text\"";
          label = "\"%{O23}%{F${black}}%{T3}%{T-}%{F-}%{F-}%{O-36}%{F${background}}%{T3}%{T-}%{F-}%{O-36}%{F${black}}%{T3}%{T-}\"";
          format = "\"<label>%{O-2}\"";
        };
        "module/even-l" = {
          type = "\"custom/text\"";
          label = "\"%{O-1}%{O1}%{B${black}}%{T3} %{T-}%{B-}%{O-25}%{T3}%{T-}\"";
          label-foreground = "\"${foreground}\"";
        };
        "module/even-r" = {
          type = "\"custom/text\"";
          label-background = "\"${black}\"";
          label-foreground = "\"${foreground}\"";
          label = "\"%{T3}%{T-}%{O-1}\"";
        };
        "module/odd-r" = {
          type = "\"custom/text\"";
          label-background = "\"${foreground}\"";
          label-foreground = "\"${black}\"";
          label = "\"%{T3}%{T-}%{O-1}\"";
        };
      };
    };
  };
}
