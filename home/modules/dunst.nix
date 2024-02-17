{ config, lib, pkgs, ... }:
let
  inherit (config.modules.dunst) enable;
  inherit (config.modules.scaling) scale xft;
  inherit (config.modules.themes) icon color-scheme;
in
{
  options.modules.dunst.enable = lib.mkEnableOption "Enable dunst module";

  config = lib.mkIf enable {
    pacman.packages = [ "extra/dunst" ];

    systemd.user.services.dunst = {
      Unit = {
        Description = "Dunst notification daemon";
        After = [ "graphical-session-pre.target" ];
        PartOf = [ "graphical-session.target" ];
      };

      Service = {
        Type = "dbus";
        BusName = "org.freedesktop.Notifications";
        ExecStart =
          let
            scriptPkg = pkgs.writeHostScriptBin "dunst-start"
              "dunst --config ${config.xdg.configHome}/dunst/dunstrc";
          in
          "${scriptPkg}/bin/dunst-start";
      };
    };

    xdg.configFile."dunst/dunstrc" = {
      text =
        let
          withScale = num: toString (if xft then num else num * scale);
        in
        ''
          [experimental]
          per_monitor_dpi="false"

          [global]
          alignment="left"
          always_run_script="true"
          browser="brave"
          class="Dunst"
          context="mod4+shift+dead_acute"
          corner_radius="8"
          dmenu="rofi -dmenu -theme "$XDG_CONFIG_HOME/rofi/launchers/type-3/style-5.rasi" -p dunst"
          ellipsize="middle"
          follow="mouse"
          font="Noto Sans ${withScale 10}"
          force_xinerama="false"
          force_xwayland="false"
          format="%s %p\n%b "
          frame_color="${color-scheme.background}"
          frame_width="2"
          height="300"
          hide_duplicate_count="false"
          history="mod4+dead_acute"
          history_length="20"
          horizontal_padding="10"
          icon_theme="Papirus-Dark"
          enable_recursive_icon_lookup=true
          icon_position="left"
          idle_threshold="120"
          ignore_dbusclose="false"
          ignore_newline="no"
          indicate_hidden="yes"
          line_height="0"
          markup="full"
          monitor="0"
          mouse_left_click="close_current"
          mouse_middle_click="do_action, close_current"
          mouse_right_click="close_all"
          notification_limit="0"
          offset="10x35" # perfect align: 21x45
          origin="top-right"
          padding="8"
          progress_bar="true"
          progress_bar_frame_width="2"
          progress_bar_height="10"
          progress_bar_max_width="300"
          progress_bar_min_width="150"
          scale="${toString (if xft then 0 else scale)}"
          separator_color="frame"
          separator_height="2"
          show_age_threshold="60"
          show_indicators="yes"
          sort="yes"
          stack_duplicates="true"
          sticky_history="yes"
          text_icon_padding="0"
          title="Dunst"
          transparency="15"
          vertical_alignment="center"
          width="300"

          [urgency_critical]
          background="${color-scheme.background}"
          foreground="${color-scheme.foreground}"
          frame_color="${color-scheme.yellow}"
          timeout="0"

          [urgency_low]
          background="${color-scheme.background}"
          foreground="${color-scheme.foreground}"
          timeout="10"

          [urgency_normal]
          background="${color-scheme.background}"
          foreground="${color-scheme.foreground}"
          frame_color="${color-scheme.foreground}"
          timeout="10"
        '';
      onChange = ''
        pkill -u "$USER" ''${VERBOSE+-e} dunst || true
      '';
    };
  };
}
