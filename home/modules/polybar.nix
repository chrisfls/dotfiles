{ config, lib, pkgs, specialArgs, ... }:
let
  inherit (config.modules.polybar) enable;

  toggle = pkgs.writeScript "toggle"
    ''
      fst=$1
      shift

      for arg in "$@"; do
        polybar-msg action "#$arg.module_toggle"
      done

      polybar-msg action $fst next
    '';
  bluetooth = pkgs.writeScript "bluetooth"
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

      polybar-msg action bluetooth hook $hook
    '';

  pipewire = pkgs.writeScript "pipewire"
    ''
      case $1 in
        "--up")
          pamixer --increase 5
          ;;
        "--down")
          pamixer --decrease 5  
          ;;
        "--mute")
          pamixer --toggle-mute
          ;;
      esac

      if [ "$(pamixer --get-volume-human)" = "muted" ]; then
        hook=1
      else
        hook=2
      fi

      polybar-msg action audio hook $hook
    '';

  dunst-toggle = pkgs.writeScript "dunst-toggle"
    ''
      dunstctl set-paused toggle
      polybar-msg action notifications next
    '';

  background = "#000000"; # backgroundBright / blackAbsolute
  foreground = "#FCFCFC"; # foreground / white
  black = "#232627"; # background / black
  red = "#ED1515"; # red
  blue = "#1D99F3"; # blue
  yellow = "#F67400"; # yellow
in
{
  options.modules.polybar.enable = lib.mkEnableOption "Enable polybar module";

  config = lib.mkIf enable {
    pacman.packages = [
      "extra/polybar"
      # "extra/yad"
      # TODO: move these packages to desktop preset
      "extra/bluez"
      "extra/bluez-utils"
      "extra/bluedevil"
    ];

    systemd.user.services.polybar = {
      Unit = {
        Description = "Polybar status bar";
        X-Restart-Triggers = "${config.xdg.configHome}/polybar/config.ini";
      };

      Service = {
        Type = "forking";

        ExecStart =
          let scriptPkg = pkgs.writeHostScriptBin "polybar-start" "polybar topbar &";
          in "${scriptPkg}/bin/polybar-start";

        Restart = "on-failure";
      };
    };

    modules.i3wm.startup = [ "systemctl --user restart polybar.service" ];

    xdg.configFile."polybar/config.ini".text =
      ''
        [settings]
        screenchange-reload="true"


        [bar/topbar]
        enable-ipc="true"
        wm-restack="i3"

        # layout
        dpi="144"
        height="18pt"
        line-size="2pt"
        padding-left="0"
        padding-right="0"
        width="100%"
        format-radius="32.0"

        # styling
        background="${background}"
        foreground="${foreground}"

        # base font
        font-0="CaskaydiaCoveNerdFontMono:size=10;4"

        # icon font
        font-1="CaskaydiaCoveNerdFontPropo:size=11;3"

        # powerline font
        font-2="CaskaydiaCoveNerdFont:size=20;6"

        # modules
        modules-left="workspaces even-l notifications menu toggle tray right title"
        modules-center=""
        modules-right="left filesystem even-r temperature odd-r memory even-r cpu odd-r wired even-r wireless odd-r bluetooth even-r audio odd-r date even-r time"


        [module/workspaces]
        type="internal/i3"
        enable-click="true"
        enable-scroll="true"
        format="%{O-1}%{O2}%{T2}<label-state>%{T-}%{O-1}"
        fuzzy-match="false"
        index-sort="true"
        label-focused="%{O12}%icon%%{O2}"
        label-focused-background="${foreground}"
        label-focused-foreground="${blue}"
        label-focused-padding="0"
        label-mode=%{T2}%mode%%{T-}
        label-mode-background="${foreground}"
        label-mode-foreground="${background}"
        label-mode-padding="0"
        label-unfocused="%{O12}%icon%%{O2}"
        label-unfocused-background="${foreground}"
        label-unfocused-foreground="${black}"
        label-unfocused-padding="0"
        label-urgent="%{O12}%icon%%{O2}"
        label-urgent-background="${foreground}"
        label-urgent-foreground="${yellow}"
        label-urgent-padding="0"
        label-visible="%{O12}%icon%%{O2}"
        label-visible-background="${foreground}"
        label-visible-foreground="${background}"
        label-visible-padding="0"
        pin-workspaces="true"
        reverse-scroll="false"
        show-urgent="true"
        strip-wsnumbers="true"
        wrapping-scroll="true"
        ws-icon-0="0;󰪥"
        ws-icon-1="1;󰲠"
        ws-icon-10="10;󰲞"
        ws-icon-2="2;󰲢"
        ws-icon-3="3;󰲤"
        ws-icon-4="4;󰲦"
        ws-icon-5="5;󰲨"
        ws-icon-6="6;󰲪"
        ws-icon-7="7;󰲬"
        ws-icon-8="8;󰲮"
        ws-icon-9="9;󰲰"
        ws-icon-default="󰝦"


        [module/menu]
        type="custom/ipc"
        click-left="rofi-menu"
        format-0="%{T2}%{T-}%{O1}"
        format-0-background="${black}"
        format-0-foreground="${foreground}"
        format-0-padding="0"
        format-1="%{T2}%{T-}%{O1}"
        format-1-background="${black}"
        format-1-foreground="${red}"
        format-1-padding="0"
        format-2="%{T2}%{T-}%{O1}"
        format-2-background="${black}"
        format-2-foreground="${blue}"
        format-2-padding="0"
        hook-0=""
        hook-1=""
        hook-2=""
        initial="1"


        [module/toggle]
        type="custom/ipc"
        click-left="${toggle} toggle tray &"
        format-0="%{T2}%{T-}"
        format-0-background="${black}"
        format-0-foreground="${foreground}"
        format-0-padding="1"
        format-1="%{T2}%{T-}"
        format-1-background="${black}"
        format-1-foreground="${background}"
        format-1-padding="1"
        hook-0=""
        hook-1=""
        initial="1"
        scroll-down="${toggle} toggle tray &"
        scroll-up="${toggle} toggle tray &"


        [module/notifications]
        type="custom/ipc"
        click-left="${dunst-toggle} &"
        click-right="rofi-dunst &"
        format-0="%{T2}%{T-}"
        format-0-background="${black}"
        format-0-foreground="${foreground}"
        format-0-padding="1"
        format-1="%{T2}%{T-}"
        format-1-background="${black}"
        format-1-foreground="${foreground}"
        format-1-padding="1"
        hidden="false"
        hook-0=""
        hook-1=""
        initial="1"
        scroll-down="dunstctl history-pop &"
        scroll-up="dunstctl close &"


        [module/tray]
        type="internal/tray"
        format="%{O-4}<tray>"
        format-background="${black}"
        hidden="false"
        tray-background="${black}"
        tray-foreground="${foreground}"
        tray-padding="1pt"
        tray-size="75%"
        tray-spacing="1pt"


        [module/title]
        format-padding="3"
        label="%title:0:100:...%"
        label-background="${background}"
        label-empty="Desktop"
        label-empty-background="${background}"
        type="internal/xwindow"


        [module/filesystem]
        type="internal/fs"
        format-mounted="<label-mounted> %{T2}󰋊%{T-} "
        format-mounted-background="${black}"
        format-mounted-foreground="${foreground}"
        format-unmounted="<label-unmounted> %{T2}󰋊%{T-} "
        format-unmounted-background="${black}"
        format-unmounted-foreground="${foreground}"
        format-warn="<label-warn> %{T2}󰋊%{T-} "
        format-warn-background="${black}"
        format-warn-foreground="${foreground}"
        label-mounted="%free%"
        label-unmounted="?"
        label-warn="%free%"
        mount-0="/"
        warn-percentage="50"


        [module/temperature]
        type="internal/temperature"
        base-temperature="20"
        format="<label> %{T2}<ramp>%{T-} "
        format-background="${foreground}"
        format-foreground="${black}"
        format-warn="<label-warn> %{T2}<ramp>%{T-} "
        format-warn-background="${foreground}"
        format-warn-foreground="${black}"
        interval="0.5"
        label="%temperature-c%"
        label-warn="%temperature-c%"
        ramp-0=""
        ramp-1=""
        ramp-2=""
        ramp-3=""
        ramp-4=""
        thermal-zone="0"
        warn-temperature="80"
        zone-type="x86_pkg_temp"


        [module/memory]
        type="internal/memory"
        format="<label> %{T2}%{T-} "
        format-background="${black}"
        format-foreground="${foreground}"
        format-warn="<label-warn> %{T2}%{T-} "
        format-warn-background="${black}"
        format-warn-foreground="${foreground}"
        label="%percentage_used%%"
        label-warn="%percentage_used%%"


        [module/cpu]
        type="internal/cpu"
        format="<ramp-load> %{T2}%{T-} "
        format-background="${foreground}"
        format-foreground="${black}"
        format-warn="%{F${red}}<ramp-load>%{F${red}} %{T2}%{T-} "
        format-warn-background="${foreground}"
        format-warn-foreground="${black}"
        interval="0.5"
        ramp-load-0="▁"
        ramp-load-1="▂"
        ramp-load-2="▃"
        ramp-load-3="▄"
        ramp-load-4="▅"
        ramp-load-5="▆"
        ramp-load-6="▇"
        ramp-load-7="█"
        warn-percentage="50"


        [module/wired]
        type="internal/network"
        format-connected="<label-connected>"
        format-connected-background="${black}"
        format-connected-foreground="${foreground}"
        format-disconnected="<label-disconnected>"
        format-disconnected-background="${black}"
        format-disconnected-foreground="${foreground}"
        interface-type="wired"
        interval="10"
        label-connected="%netspeed% %{T2}󰈁%{T-} "
        label-disconnected="off  %{T2}󰈂{T-} "


        [module/wireless]
        type="internal/network"
        format-connected="<label-connected> %{T2}<ramp-signal>%{T-} "
        format-connected-background="${foreground}"
        format-connected-foreground="${black}"
        format-disconnected="<label-disconnected> %{T2}󰤠{T-} "
        format-disconnected-background="${foreground}"
        format-disconnected-foreground="${black}"
        interface-type="wireless"
        interval="2"
        label-connected="%signal%%"
        label-disconnected="off"
        ramp-signal-0="󰤯"
        ramp-signal-1="󰤟"
        ramp-signal-2="󰤢"
        ramp-signal-3="󰤥"
        ramp-signal-4="󰤨"


        [module/bluetooth]
        type="custom/ipc"
        click-left="${bluetooth} --toggle &"
        click-right="bluedevil-wizard &"
        format-1="%{T2}󰂲%{T-} "
        format-1-background="${black}"
        format-1-foreground="${foreground}"
        format-2="%{T2}%{T-} "
        format-2-background="${black}"
        format-2-foreground="${foreground}"
        hook-0="${bluetooth} &"
        hook-1=""
        hook-2=""
        initial="1"
        label="%output% "


        [module/audio]
        type="custom/ipc"
        click-left="${pipewire} --mute &"
        click-right="pavucontrol-qt &"
        format-1="<label> %{T2}󰝟%{T-} "
        format-1-background="${foreground}"
        format-1-foreground="${black}"
        format-2="<label> %{T2}󰕾%{T-} "
        format-2-background="${foreground}"
        format-2-foreground="${black}"
        hook-0="${pipewire} &"
        hook-1="pamixer --get-volume"
        hook-2="pamixer --get-volume"
        initial="1"
        label="%output%%"
        scroll-down="${pipewire} --down &"
        scroll-up="${pipewire} --up &"


        [module/date]
        type="internal/date"
        format-background="${black}"
        format-foreground="${foreground}"
        interval="1.0"
        label="%time% "
        time="%a, %d %b %Y %{T2}󰃭%{T-}"


        [module/time]
        type="internal/date"
        format-background="${foreground}"
        format-foreground="${black}"
        interval="1.0"
        label="%time% "
        time="%H:%M:%S %{T2}󰥔%{T-}"


        [module/menu-r]
        type="custom/text"
        label="%{T3}%{T-}%{O-1}"
        label-background="${foreground}"
        label-foreground="${background}"


        [module/menu-l]
        type="custom/text"
        label="%{O-1}%{O1}%{B${foreground}}%{T3} %{T-}%{B-}%{O-25}%{T3}%{T-}"
        label-foreground="${background}"


        [module/left]
        type="custom/text"
        format="<label>%{O-2}"
        label="%{F${black}}%{T3}%{T-}%{F-}%{O-12}%{F${background}}%{T3}%{T-}%{F-}%{O-12}%{F${black}}%{T3}%{T-}%{F-}"


        [module/right]
        type="custom/text"
        format="<label>%{O-2}"
        label="%{O23}%{F${black}}%{T3}%{T-}%{F-}%{F-}%{O-36}%{F${background}}%{T3}%{T-}%{F-}%{O-36}%{F${black}}%{T3}%{T-}"


        [module/even-l]
        type="custom/text"
        label="%{O-1}%{O1}%{B${black}}%{T3} %{T-}%{B-}%{O-25}%{T3}%{T-}"
        label-foreground="${foreground}"


        [module/even-r]
        type="custom/text"
        label="%{T3}%{T-}%{O-1}"
        label-background="${black}"
        label-foreground="${foreground}"


        [module/odd-r]
        type="custom/text"
        label="%{T3}%{T-}%{O-1}"
        label-background="${foreground}"
        label-foreground="${black}"
      '';
  };
}
