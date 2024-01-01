{ config, lib, pkgs, ... }:
let
  cfg = config.module.i3wm;

  colors = config.module.themes.color-scheme;

  mod = config.xsession.windowManager.i3.config.modifier;

  mouse-warp-pkg = pkgs.writeShellScriptBin "mouse-warp"
    ''
      eval $(${pkgs.xdotool}/bin/xdotool getwindowfocus getwindowgeometry --shell)
      ${pkgs.xdotool}/bin/xdotool mousemove $((X + (WIDTH / 2))) $((Y + (HEIGHT / 2)))
    '';

  mouse-warp = "exec \"${mouse-warp-pkg}/bin/mouse-warp\"";
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
            background = colors.foreground;
            border = colors.foreground;
            childBorder = colors.foreground;
            indicator = colors.blueBright;
            text = colors.background;
          };
          focusedInactive = {
            background = colors.blackBright;
            border = colors.blackBright;
            childBorder = colors.blackBright;
            indicator = colors.blue;
            text = colors.background;
          };
          placeholder = {
            background = colors.background;
            border = colors.background;
            childBorder = colors.background;
            indicator = colors.blue;
            text = colors.foreground;
          };
          unfocused = {
            background = colors.background;
            border = colors.background;
            childBorder = colors.background;
            indicator = colors.blue;
            text = colors.black;
          };
          urgent = {
            background = colors.background;
            border = colors.background;
            childBorder = colors.background;
            indicator = colors.blue;
            text = colors.yellow;
          };
        };
        defaultWorkspace = "1";
        floating = {
          border = 1;
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
          # smartBorders = "on";
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
          insert = {
            "h" = "focus child; move left; mode default";
            "j" = "focus child; move down; mode default";
            "k" = "focus child; move up; mode default";
            "l" = "focus child; move right; mode default";

            "Left" = "focus child; move left; mode default";
            "Down" = "focus child; move down; mode default";
            "Up" = "focus child; move up; mode default";
            "Right" = "focus child; move right; mode default";

            "Escape" = "mode default";
            "Return" = "mode default";
            "i" = "mode default";
            "${mod}+i" = "mode default";
          };
        };
        startup = [ ];
        window = {
          border = 1;
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
          "${mod}+t" = "focus child; layout tabbed";

          # toggle [f]loating state
          "${mod}+f" = "floating toggle";

          # [r]otate layout
          "${mod}+r" = "layout toggle splitv splith";

          "${mod}+s" = "mode resize";
          "${mod}+i" = "mode insert";

          # ## #
          # tertiary actions (show desktop/equalize/balance)
          # ## #

          # TODO: toggle desktop

          "${mod}+p" = "focus parent";
          "${mod}+g" = "gaps inner toggle; gaps outer toggle";

          ######## #### ## #
          # FOCUS AND MOVEMENT
          ######## #### ## #

          # focus with vim keys
          "${mod}+h" = "focus left; ${mouse-warp}";
          "${mod}+j" = "focus down; ${mouse-warp}";
          "${mod}+k" = "focus up; ${mouse-warp}";
          "${mod}+l" = "focus right; ${mouse-warp}";

          # focus with arrow keys
          "${mod}+Left" = "focus left; ${mouse-warp}";
          "${mod}+Down" = "focus down; ${mouse-warp}";
          "${mod}+Up" = "focus up; ${mouse-warp}";
          "${mod}+Right" = "focus right; ${mouse-warp}";

          # move with vim keys
          "${mod}+Shift+h" = "mark swap; focus left; swap container with mark swap; [con_mark=\"swap\"] focus; unmark swap; ${mouse-warp}";
          "${mod}+Shift+j" = "mark swap; focus down; swap container with mark swap; [con_mark=\"swap\"] focus; unmark swap; ${mouse-warp}";
          "${mod}+Shift+k" = "mark swap; focus up; swap container with mark swap; [con_mark=\"swap\"] focus; unmark swap; ${mouse-warp}";
          "${mod}+Shift+l" = "mark swap; focus right; swap container with mark swap; [con_mark=\"swap\"] focus; unmark swap; ${mouse-warp}";

          # move with arrow keys
          "${mod}+Shift+Left" = "mark swap; focus left; swap container with mark swap; [con_mark=\"swap\"] focus; unmark swap; ${mouse-warp}";
          "${mod}+Shift+Down" = "mark swap; focus down; swap container with mark swap; [con_mark=\"swap\"] focus; unmark swap; ${mouse-warp}";
          "${mod}+Shift+Up" = "mark swap; focus up; swap container with mark swap; [con_mark=\"swap\"] focus; unmark swap; ${mouse-warp}";
          "${mod}+Shift+Right" = "mark swap; focus right; swap container with mark swap; [con_mark=\"swap\"] focus; unmark swap; ${mouse-warp}";

          # focus floating windows
          "${mod}+space" = "focus mode_toggle";
          "${mod}+o" = "mark subwindow; focus parent; focus parent; mark parent; [con_mark=\"subwindow\"] focus; move window to mark parent; [con_mark=\"subwindow\"] focus; unmark";

          ######## #### ## #
          # WORKSPACES
          ######## #### ## #

          "${mod}+apostrophe" = "workspace number 0; ${mouse-warp}";
          "${mod}+1" = "workspace number 1; ${mouse-warp}";
          "${mod}+2" = "workspace number 2; ${mouse-warp}";
          "${mod}+3" = "workspace number 3; ${mouse-warp}";
          "${mod}+4" = "workspace number 4; ${mouse-warp}";
          "${mod}+5" = "workspace number 5; ${mouse-warp}";
          "${mod}+6" = "workspace number 6; ${mouse-warp}";
          "${mod}+7" = "workspace number 7; ${mouse-warp}";
          "${mod}+8" = "workspace number 8; ${mouse-warp}";
          "${mod}+9" = "workspace number 9; ${mouse-warp}";
          "${mod}+0" = "workspace number 10; ${mouse-warp}";
          "${mod}+m" = "scratchpad show";

          "${mod}+Shift+apostrophe" = "move container to workspace number 0; ${mouse-warp}";
          "${mod}+Shift+1" = "move container to workspace number 1; ${mouse-warp}";
          "${mod}+Shift+2" = "move container to workspace number 2; ${mouse-warp}";
          "${mod}+Shift+3" = "move container to workspace number 3; ${mouse-warp}";
          "${mod}+Shift+4" = "move container to workspace number 4; ${mouse-warp}";
          "${mod}+Shift+5" = "move container to workspace number 5; ${mouse-warp}";
          "${mod}+Shift+6" = "move container to workspace number 6; ${mouse-warp}";
          "${mod}+Shift+7" = "move container to workspace number 7; ${mouse-warp}";
          "${mod}+Shift+8" = "move container to workspace number 8; ${mouse-warp}";
          "${mod}+Shift+9" = "move container to workspace number 9; ${mouse-warp}";
          "${mod}+Shift+0" = "move container to workspace number 10; ${mouse-warp}";
          "${mod}+shift+m" = "move scratchpad";
        };
      };
    };

    services.polybar = {
      package = pkgs.polybarFull;
      settings = {
        "bar/topbar".wm-restack = "\"i3\"";
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

          format = "\"%{O-1}%{O2}%{T2}<label-state>%{T-}%{}%{O-1}\"";
          label-mode = "%{T2}%mode%%{T-}";
          label-mode-background = "\"${colors.foreground}\"";
          label-mode-foreground = "\"${colors.background}\"";
          label-mode-padding = "\"0\"";

          label-focused = "\"%{O12}%icon%%{O2}\"";
          label-focused-background = "\"${colors.foreground}\"";
          label-focused-foreground = "\"${colors.blueBright}\"";
          label-focused-padding = "\"0\"";

          label-visible = "\"%{O12}%icon%%{O2}\"";
          label-visible-background = "\"${colors.foreground}\"";
          label-visible-foreground = "\"${colors.background}\"";
          label-visible-padding = "\"0\"";

          label-unfocused = "\"%{O12}%icon%%{O2}\"";
          label-unfocused-background = "\"${colors.foreground}\"";
          label-unfocused-foreground = "\"${colors.black}\"";
          label-unfocused-padding = "\"0\"";

          label-urgent = "\"%{O12}%icon%%{O2}\"";
          label-urgent-background = "\"${colors.foreground}\"";
          label-urgent-foreground = "\"${colors.yellowBright}\"";
          label-urgent-padding = "\"0\"";

          ws-icon-0 = "\"0;󰪥\"";
          ws-icon-1 = "\"1;󰲠\"";
          ws-icon-2 = "\"2;󰲢\"";
          ws-icon-3 = "\"3;󰲤\"";
          ws-icon-4 = "\"4;󰲦\"";
          ws-icon-5 = "\"5;󰲨\"";
          ws-icon-6 = "\"6;󰲪\"";
          ws-icon-7 = "\"7;󰲬\"";
          ws-icon-8 = "\"8;󰲮\"";
          ws-icon-9 = "\"9;󰲰\"";
          ws-icon-10 = "\"10;󰲞\"";
          ws-icon-default = "\"󰝦\"";
        };
      };
    };
  };
}
