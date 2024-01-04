{ config, lib, pkgs, ... }:
let
  inherit (builtins) concatStringsSep;

  cfg = config.module.i3wm;

  colors = config.module.themes.color-scheme;

  script = name: "exec \"$SCRIPT/${name}\"";

  i3-msg = "${pkgs.i3}/bin/i3-msg";

  jaq = "${pkgs.usr.jaq}/bin/jaq";

  xdotool = "${pkgs.usr.xdotool}/bin/xdotool";

  mod = config.xsession.windowManager.i3.config.modifier;
in
{
  options.module.i3wm.enable = lib.mkEnableOption "Enable i3wm module";

  config = lib.mkIf cfg.enable {
    module.script.enable = true;

    module.script.install = {
      autotile =
        ''
          ${i3-msg} -t subscribe -m '[ "window", "binding" ]' | while IFS= read -r line; do
            sleep ${toString (1.0 / 30.0)}

            layout=$(${i3-msg} -t get_tree | ${jaq} -r 'recurse(.nodes[];.nodes!=null)|select(.nodes[].focused).layout')

            if [ "$layout" = "tabbed" ]; then
              continue
            fi

            eval $(${xdotool} getwindowfocus getwindowgeometry --shell)

            if [ "$WIDTH" -gt "$HEIGHT" ]; then
              ${i3-msg} "split horizontal"
            else
              ${i3-msg} "split vertical"
            fi
          done
        '';

      mouse-warp =
        ''
          eval $(${xdotool} getwindowfocus getwindowgeometry --shell)
          ${xdotool} mousemove $((X + (WIDTH / 2))) $((Y + (HEIGHT / 2)))
        '';
    };

    # pacman.overrides.i3 = [ "extra/i3wm" ];

    xsession.windowManager.i3 = {
      enable = true;
      config = {
        modifier = "Mod4";
        bars = [ ];
        colors = {
          background = colors.background;
          focused = {
            background = colors.foreground;
            border = colors.foreground;
            childBorder = colors.foreground;
            indicator = colors.foreground;
            text = colors.background;
          };
          focusedInactive = {
            background = colors.black;
            border = colors.black;
            childBorder = colors.black;
            indicator = colors.black;
            text = colors.background;
          };
          placeholder = {
            background = colors.background;
            border = colors.background;
            childBorder = colors.background;
            indicator = colors.background;
            text = colors.foreground;
          };
          unfocused = {
            background = colors.background;
            border = colors.background;
            childBorder = colors.background;
            indicator = colors.background;
            text = colors.black;
          };
          urgent = {
            background = colors.background;
            border = colors.background;
            childBorder = colors.background;
            indicator = colors.background;
            text = colors.yellow;
          };
        };
        defaultWorkspace = "1";
        floating = {
          border = 2;
          criteria = [ ];
          modifier = mod;
          titlebar = false;
        };
        focus = {
          followMouse = false;
          newWindow = "focus";
          wrapping = "workspace";
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
            "h" = "focus child; move left; mode default; ${script "mouse-warp"}";
            "j" = "focus child; move down; mode default; ${script "mouse-warp"}";
            "k" = "focus child; move up; mode default; ${script "mouse-warp"}";
            "l" = "focus child; move right; mode default; ${script "mouse-warp"}";

            "Left" = "focus child; move left; mode default; ${script "mouse-warp"}";
            "Down" = "focus child; move down; mode default; ${script "mouse-warp"}";
            "Up" = "focus child; move up; mode default; ${script "mouse-warp"}";
            "Right" = "focus child; move right; mode default; ${script "mouse-warp"}";

            "Escape" = "mode default";
            "Return" = "mode default";
            "i" = "mode default";
            "${mod}+i" = "mode default";
          };
        };
        startup = [
          {
            command = "$SCRIPT/autotile";
            always = true;
            notification = false;
          }
        ];
        window = {
          border = 2;
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
        workspaceLayout = "default";
        keybindings = {
          ######## #### ## #
          # MISC
          ######## #### ## #

          # reload config
          "${mod}+Escape" = "reload";

          # restart wm
          "${mod}+Shift+Escape" = "restart";

          # quit sessionss
          "Control+Mod1+Escape" = "exec i3-nagbar -t warning -m 'Do you want to exit i3?' -b 'Yes' 'i3-msg exit'";

          # open terminal
          "${mod}+BackSpace" = "exec ${config.xsession.windowManager.i3.config.terminal}";

          # open menu
          "${mod}+Return" = "exec ${config.xsession.windowManager.i3.config.menu}";
          "${mod}+semicolon" = "exec ${config.xsession.windowManager.i3.config.menu}";

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
          "${mod}+t" = "focus child; layout toggle tabbed split";

          # toggle [f]loating state
          "${mod}+f" = "floating toggle";

          # [r]otate layout
          "${mod}+r" = "focus child; layout toggle splitv splith";

          "${mod}+s" = "mode resize";
          "${mod}+i" = "mode insert";

          # ## #
          # tertiary actions (show desktop/equalize/balance)
          # ## #

          # TODO: toggle desktop

          "${mod}+p" = "focus parent";
          "${mod}+Shift+p" = "focus child";

          "${mod}+g" = "gaps inner toggle; gaps outer toggle";

          ######## #### ## #
          # FOCUS AND MOVEMENT
          ######## #### ## #

          # focus with vim keys
          "${mod}+h" = "focus left; ${script "mouse-warp"}";
          "${mod}+j" = "focus down; ${script "mouse-warp"}";
          "${mod}+k" = "focus up; ${script "mouse-warp"}";
          "${mod}+l" = "focus right; ${script "mouse-warp"}";

          # focus with arrow keys
          "${mod}+Left" = "focus left; ${script "mouse-warp"}";
          "${mod}+Down" = "focus down; ${script "mouse-warp"}";
          "${mod}+Up" = "focus up; ${script "mouse-warp"}";
          "${mod}+Right" = "focus right; ${script "mouse-warp"}";

          # move with vim keys
          "${mod}+Shift+h" = "mark swap; focus left; swap container with mark swap; [con_mark=\"swap\"] focus; unmark swap; ${script "mouse-warp"}";
          "${mod}+Shift+j" = "mark swap; focus down; swap container with mark swap; [con_mark=\"swap\"] focus; unmark swap; ${script "mouse-warp"}";
          "${mod}+Shift+k" = "mark swap; focus up; swap container with mark swap; [con_mark=\"swap\"] focus; unmark swap; ${script "mouse-warp"}";
          "${mod}+Shift+l" = "mark swap; focus right; swap container with mark swap; [con_mark=\"swap\"] focus; unmark swap; ${script "mouse-warp"}";

          # move with arrow keys
          "${mod}+Shift+Left" = "mark swap; focus left; swap container with mark swap; [con_mark=\"swap\"] focus; unmark swap; ${script "mouse-warp"}";
          "${mod}+Shift+Down" = "mark swap; focus down; swap container with mark swap; [con_mark=\"swap\"] focus; unmark swap; ${script "mouse-warp"}";
          "${mod}+Shift+Up" = "mark swap; focus up; swap container with mark swap; [con_mark=\"swap\"] focus; unmark swap; ${script "mouse-warp"}";
          "${mod}+Shift+Right" = "mark swap; focus right; swap container with mark swap; [con_mark=\"swap\"] focus; unmark swap; ${script "mouse-warp"}";

          # focus floating windows
          "${mod}+space" = "focus mode_toggle; ${script "mouse-warp"}";
          "${mod}+o" = "mark subwindow; focus parent; focus parent; mark parent; [con_mark=\"subwindow\"] focus; move window to mark parent; [con_mark=\"subwindow\"] focus; unmark";

          ######## #### ## #
          # WORKSPACES
          ######## #### ## #

          "${mod}+apostrophe" = "workspace number 0; ${script "mouse-warp"}";
          "${mod}+1" = "workspace number 1; ${script "mouse-warp"}";
          "${mod}+2" = "workspace number 2; ${script "mouse-warp"}";
          "${mod}+3" = "workspace number 3; ${script "mouse-warp"}";
          "${mod}+4" = "workspace number 4; ${script "mouse-warp"}";
          "${mod}+5" = "workspace number 5; ${script "mouse-warp"}";
          "${mod}+6" = "workspace number 6; ${script "mouse-warp"}";
          "${mod}+7" = "workspace number 7; ${script "mouse-warp"}";
          "${mod}+8" = "workspace number 8; ${script "mouse-warp"}";
          "${mod}+9" = "workspace number 9; ${script "mouse-warp"}";
          "${mod}+0" = "workspace number 10; ${script "mouse-warp"}";
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

    services.polybar.settings = {
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

        format = "\"%{O-1}%{O2}%{T2}<label-state>%{T-}%{O-1}\"";
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
}
