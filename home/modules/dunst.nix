{ config, lib, pkgs, ... }:
let
  inherit (config.modules.dunst) enable;
  inherit (config.modules.scaling) scale;
  inherit (config.modules.themes) icon color-scheme;
in
{
  options.modules.dunst.enable = lib.mkEnableOption "Enable dunst module";

  config = lib.mkIf enable {
    # home manager fix
    systemd.user.services.dunst.Service.Environment = lib.mkIf (config.preset.non-nixos) (lib.mkForce [ ]);

    services.dunst = {
      enable = true;

      iconTheme = {
        name = icon.name;
        package = icon.package;
        size =
          let size = toString (builtins.floor (32 * scale));
          in "${size}x${size}";
      };

      # forked from https://github.com/dracula/dunst/blob/master/dunstrc
      settings = {
        global = {
          history = "mod4+dead_acute";
          context = "mod4+shift+dead_acute";
          monitor = "0";
          follow = "mouse";
          width = "300";
          height = "300";
          origin = "top-right";
          offset = "10x35"; # perfect align: 21x45
          scale = "0";
          notification_limit = "0";
          progress_bar = "true";
          progress_bar_height = "10";
          progress_bar_frame_width = "2";
          progress_bar_min_width = "150";
          progress_bar_max_width = "300";
          indicate_hidden = "yes";
          transparency = "15";
          separator_height = "2";
          padding = "8";
          horizontal_padding = "10";
          text_icon_padding = "0";
          frame_width = "2";
          frame_color = color-scheme.background;
          separator_color = "frame";
          sort = "yes";
          idle_threshold = "120";
          font = "Noto Sans 10";
          line_height = "0";
          markup = "full";
          format = "%s %p\\n%b ";
          alignment = "left";
          vertical_alignment = "center";
          show_age_threshold = "60";
          ellipsize = "middle";
          ignore_newline = "no";
          stack_duplicates = "true";
          hide_duplicate_count = "false";
          show_indicators = "yes";
          icon_position = "left";
          sticky_history = "yes";
          history_length = "20";
          dmenu = "rofi -dmenu -theme \"${config.xdg.configHome}/rofi/launchers/type-3/style-5.rasi\" -p dunst";
          browser = "brave";
          always_run_script = "true";
          title = "Dunst";
          class = "Dunst";
          corner_radius = "8";
          ignore_dbusclose = "false";
          force_xwayland = "false";
          force_xinerama = "false";
          mouse_left_click = "close_current";
          mouse_middle_click = "do_action, close_current";
          mouse_right_click = "close_all";
        };
        experimental.per_monitor_dpi = "false";
        urgency_low = {
          background = color-scheme.background;
          foreground = color-scheme.foreground;
          timeout = "10";
        };
        urgency_normal = {
          background = color-scheme.background;
          foreground = color-scheme.foreground;
          frame_color = color-scheme.foreground;
          timeout = "10";
        };
        urgency_critical = {
          background = color-scheme.background;
          foreground = color-scheme.foreground;
          frame_color = color-scheme.yellow;
          timeout = "0";
        };
      };
    };
  };
}
