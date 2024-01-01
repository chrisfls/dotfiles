{ config, lib, pkgs, ... }:
let
  cfg = config.module.i3wm;

  colors = config.module.themes.color-scheme;
  light = colors.foreground;
  dark = colors.black;
  select = colors.blueBright;
  warning = colors.yellowBright;
in
{
  options.module.i3wm.enable = lib.mkEnableOption "Enable i3wm module";

  config = lib.mkIf cfg.enable {
    xsession.windowManager.i3 = {
      enable = true;
      config = {
        modifier = "Mod4";
        terminal = "wezterm start";
        menu = "rofi-menu";
        bars = [ ];
        colors = {
          #background = "";
          #focused = "";
          #focusedInactive = "";
          #placeholder = "";
          #unfocused = "";
          #urgent = "";
        };
        defaultWorkspace = "1";
        floating = {
          border = 3;
          criteria = [ ];
          modifier = "Mod4";
          titlebar = false;
        };
        focus = {
          followMouse = false;
          newWindow = "smart";
          wrapping = "yes";
        };
        /*fonts = {
            names = [ "DejaVu Sans Mono" "FontAwesome5Free" ];
            style = "Bold Semi-Condensed";
            size = 11.0;
          };*/
        gaps = {
          top = 0;
          bottom = 0;
          left = 0;
          right = 0;
          horizontal = 0;
          vertical = 0;
          inner = 16;
          outer = 32;
          smartBorders = "on";
          smartGaps = true;
        };
        modes = {
          resize = {
            Down = "resize grow height 10 px or 10 ppt";
            Escape = "mode default";
            Left = "resize shrink width 10 px or 10 ppt";
            Return = "mode default";
            Right = "resize grow width 10 px or 10 ppt";
            Up = "resize shrink height 10 px or 10 ppt";
          };
        };
        startup = [
        ];
        window = {
          border = 3;
          /*
          commands = [
            {
              command = "border pixel 1";
              criteria = {
                class = "XTerm";
              };
            }
          ];
          */
          hideEdgeBorders = "smart";
          titlebar = false;
        };
        workspaceAutoBackAndForth = true;
        workspaceOutputAssign = {
          #.*.workspace
        };
      };
    };

    services.polybar.settings = {
      "module/workspaces" = {
        type = "\"internal/i3\"";

        pin-workspaces = "\"true\"";
        show-urgent = "\"true\"";
        strip-wsnumbers = "\"true\"";
        index-sort = "\"true\"";
        enable-click = "\"true\"";
        enable-scroll = "\"true\"";
        wrapping-scroll = "\"true\"";
        reverse-scroll = "\"false\"";
        fuzzy-match = "\"false\"";

        format = "\"%{O-1}%{T1}%{O2}<label-state>%{T-} <label-mode>%{O-12}\"";

        label-mode = "%mode%";
        label-mode-padding = "1";

        label-focused = "";
        label-focused-background = "\"${light}\"";
        label-focused-foreground = "\"${select}\"";
        label-focused-padding = "\"1\"";

        label-visible = "";
        label-visible-background = "\"${light}\"";
        label-visible-foreground = "\"${select}\"";
        label-visible-padding = "\"1\"";

        label-unfocused = "\"\"";
        label-unfocused-background = "\"${light}\"";
        label-unfocused-foreground = "\"${dark}\"";
        label-unfocused-padding = "\"1\"";

        label-urgent = "\"\"";
        label-urgent-background = "\"${light}\"";
        label-urgent-foreground = "\"${warning}\"";
        label-urgent-padding = "\"1\"";
      };
    };
  };
}
