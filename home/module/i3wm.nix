{ config, lib, pkgs, ... }:
let
  cfg = config.module.i3wm;

  colors = config.module.themes.color-scheme;
  light = colors.foreground;
  dark = colors.black;
  select = colors.blueBright;
  warning = colors.yellowBright;

  mod = config.xsession.windowManager.i3.config.modifier;
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
          background = colors.background;
          focused = {
            background = "#ffffff";
            border = "#ffffff";
            childBorder = colors.background;
            indicator = colors.red;
            text = colors.background;
          };
          focusedInactive = {
            background = colors.white;
            border = colors.white;
            childBorder = colors.background;
            indicator = colors.red;
            text = colors.background;
          };
          placeholder = {
            background = colors.background;
            border = colors.background;
            childBorder = colors.background;
            indicator = colors.red;
            text = colors.foreground;
          };
          unfocused = {
            background = colors.background;
            border = colors.background;
            childBorder = colors.background;
            indicator = colors.red;
            text = colors.blackBright;
          };
          urgent = {
            background = colors.background;
            border = colors.background;
            childBorder = colors.background;
            indicator = colors.red;
            text = colors.yellowBright;
          };
        };
        defaultWorkspace = "1";
        floating = {
          border = 3;
          criteria = [ ];
          modifier = mod;
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
            "h" = "resize shrink width 100 px or 10 ppt";
            "j" = "resize grow height 100 px or 10 ppt";
            "k" = "resize shrink height 100 px or 10 ppt";
            "l" = "resize grow width 100 px or 10 ppt";

            "Left" = "resize shrink width 100 px or 10 ppt";
            "Down" = "resize grow height 100 px or 10 ppt";
            "Up" = "resize shrink height 100 px or 10 ppt";
            "Right" = "resize grow width 100 px or 10 ppt";
            
            "Escape" = "mode default";
            "Return" = "mode default";
            "r" = "mode default";
          };
        };
        startup = [ ];
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
        workspaceLayout = "tabbed";
        keybindings = {
          ######## #### ## #
          # MISC
          ######## #### ## #

          # reload config
          "${mod}+Escape" = "reload";

          # restart wm
          "${mod}+Shift+r" = "restart";

          # quit session
          "${mod}+Shift+e" =
            "exec i3-nagbar -t warning -m 'Do you want to exit i3?' -b 'Yes' 'i3-msg exit'";

          # TODO: move to modules
          "${mod}+BackSpace" = "exec ${config.xsession.windowManager.i3.config.terminal}";
          "${mod}+Return" = "exec ${config.xsession.windowManager.i3.config.menu}";

          ######## #### ## #
          # WINDOW CONTROLS
          ######## #### ## #

          # ## #
          # primary actions (close/maximize)
          # ## #

          # [c]lose app
          "${mod}+c" = "kill";
          # [c]lose app (kill) [TODO: check if appliable]
          # "${mod}+Shift+c" = "kill";

          # toggle ma[x]imize state (fullscreen)
          "${mod}+x" = "fullscreen toggle";

          # ## #
          # secondary actions (layout/pseudo float/fix rotate brother)
          # ## #

          # cycle [t]iled/monocle layout
          "${mod}+t" = "layout tabbed";

          # toggle [f]loating state
          "${mod}+f" = "floating toggle";

          # [r]otate layout
          "${mod}+r" = "layout toggle splitv splith";

          "${mod}+s" = "mode resize";

          # ## #
          # tertiary actions (show desktop/equalize/balance)
          # ## #

          # TODO: toggle desktop

          "${mod}+p" = "focus parent";

          # TODO: toggle gaps 

          ######## #### ## #
          # FOCUS AND MOVEMENT
          ######## #### ## #

          # focus with vim keys
          "${mod}+h" = "focus left";
          "${mod}+j" = "focus down";
          "${mod}+k" = "focus up";
          "${mod}+l" = "focus right";

          # focus with arrow keys
          "${mod}+Left" = "focus left";
          "${mod}+Down" = "focus down";
          "${mod}+Up" = "focus up";
          "${mod}+Right" = "focus right";

          # move with vim keys
          "${mod}+Shift+h" = "mark swap; focus left; swap container with mark swap; unmark swap";
          "${mod}+Shift+j" = "mark swap; focus down; swap container with mark swap; unmark swap";
          "${mod}+Shift+k" = "mark swap; focus up; swap container with mark swap; unmark swap";
          "${mod}+Shift+l" = "mark swap; focus right; swap container with mark swap; unmark swap";

          # move with arrow keys
          "${mod}+Shift+Left" = "mark swap; focus left; swap container with mark swap; unmark swap";
          "${mod}+Shift+Down" = "mark swap; focus down; swap container with mark swap; unmark swap";
          "${mod}+Shift+Up" = "mark swap; focus up; swap container with mark swap; unmark swap";
          "${mod}+Shift+Right" = "mark swap; focus right; swap container with mark swap; unmark swap";

          # focus floating windows
          "${mod}+space" = "focus mode_toggle";

          ######## #### ## #
          # WORKSPACES
          ######## #### ## #

          "${mod}+apostrophe" = "workspace number 0";
          "${mod}+1" = "workspace number 1";
          "${mod}+2" = "workspace number 2";
          "${mod}+3" = "workspace number 3";
          "${mod}+4" = "workspace number 4";
          "${mod}+5" = "workspace number 5";
          "${mod}+6" = "workspace number 6";
          "${mod}+7" = "workspace number 7";
          "${mod}+8" = "workspace number 8";
          "${mod}+9" = "workspace number 9";
          "${mod}+0" = "workspace number 10";
          "${mod}+m" = "scratchpad show";

          "${mod}+Shift+apostrophe" = "move container to workspace number 0";
          "${mod}+Shift+1" = "move container to workspace number 1";
          "${mod}+Shift+2" = "move container to workspace number 2";
          "${mod}+Shift+3" = "move container to workspace number 3";
          "${mod}+Shift+4" = "move container to workspace number 4";
          "${mod}+Shift+5" = "move container to workspace number 5";
          "${mod}+Shift+6" = "move container to workspace number 6";
          "${mod}+Shift+7" = "move container to workspace number 7";
          "${mod}+Shift+8" = "move container to workspace number 8";
          "${mod}+Shift+9" = "move container to workspace number 9";
          "${mod}+Shift+0" = "move container to workspace number 10";
          "${mod}+shift+m" = "move scratchpad";
        };
      };
    };

    services.polybar.settings."bar/topbar".wm-restack = "\"i3\"";

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
