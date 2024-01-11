{ config, lib, pkgs, ... }:
let
  inherit (config.modules.i3wm) enable;
  inherit (builtins) concatStringsSep;
  inherit (config.xsession.windowManager.i3.config) terminal menu;

  mod = config.xsession.windowManager.i3.config.modifier;
  alt = "Mod1";
  outer = toString config.xsession.windowManager.i3.config.gaps.outer;
  inner = toString config.xsession.windowManager.i3.config.gaps.inner;
  colors = config.modules.themes.color-scheme;

  script = name: "exec --no-startup-id \"$SCRIPT/${name}\"";

  focus = dir:
    "focus ${dir}; ${script "cursor-warp"}";

  move = dir:
    "mark swap; focus ${dir}; swap container with mark swap; [con_mark=\"swap\"] focus; unmark swap; ${script "cursor-warp"}";

  workspace = num:
    "workspace number ${toString num}; ${script "cursor-warp"}";

  get-empty-space =
    ''
      i3-msg -t get_tree | jaq -r 'recurse(.nodes[];.nodes!=null)|
        select(
        .window == null and
        .type == "con" and
        .current_border_width == -1 and
        .rect.width != 0 and
        .rect.height != 0 and
        .geometry.x == 0 and
        .geometry.y == 0 and
        .geometry.width == 0 and
        .geometry.height == 0 and
        .nodes == []
        ).id'
    '';
in
{
  options.modules.i3wm.enable = lib.mkEnableOption "Enable i3wm module";

  config = lib.mkIf enable {
    modules.script.enable = true;

    modules.script.install = {
      cursor-warp = # move mouse to center of the window
        ''
          eval $(xdotool getwindowfocus getwindowgeometry --shell)
          xdotool mousemove $((X + (WIDTH / 2))) $((Y + (HEIGHT / 2)))
        '';

      toggle-split = # toggle split layout
        # tries to toggle an actual node
        ''
          id=$(i3-msg -t get_tree | jaq -r 'recurse(.nodes[];.nodes!=null)|select(.nodes[].focused or .focused).id')
          if [ "$id" ]; then
            i3-msg "[con_id=\"$id\"] layout toggle splitv splith"
          fi
        '';

      select-split = # select split direction
        # not available in tabbed containers
        ''
          id=$(recurse(.nodes[];.nodes!=null)|select(.nodes[].focused and .layout != "tabbed").id)
          if [ "$id" ]; then
            i3-msg "[con_id=\"$id\"] split toggle"
          fi
        '';

      toggle-tabs = # toggle node layout
        # toggle tabbed layout at the selected leaf
        ''
          id=$(i3-msg -t get_tree | jaq -r 'recurse(.nodes[];.nodes!=null)|select(.nodes == [] and .focused).id')
          if [ "$id" ]; then
            i3-msg "[con_id=\"$id\"] layout toggle tabbed split"
          fi
        '';

      make-space = # create empty space for a container
        ''
          id=$(${get-empty-space})
          if [ "$id" ]; then
            i3-msg "[con_id=\"$id\"] kill"
          fi

          i3-msg "mark focus; exec i3 open; [con_mark=\"focus\"] focus; unmark focus"
        '';

      fill-space = # fill existing empty space with focused container
        ''
          id=$(${get-empty-space})
          if [ "$id" ]; then
            i3-msg "swap container with con_id $id; [con_id=\"$id\"] kill"
          fi
        '';

      daemon = # makes i3 more like bspwm
        # auto splits to the best direction
        # and move new windows to empty spaces
        let
          # avoid https://github.com/i3/i3/issues/5447
          mouse-check =
            ''
              if [ -z "$(xinput --query-state $mouse | grep 'button\[3\]=up')" ]; then
                continue
              fi
            '';
        in
        ''
          mouse=$(xinput --list | grep -i -m 1 'Logitech USB Optical Mouse' | grep -o 'id=[0-9]\+' | grep -o '[0-9]\+')

          i3-msg -t subscribe -m '[ "window", "binding", "mode" ]' | while IFS= read -r line; do
            event=$(echo $line | jaq -r '.change')

            case "$event" in
              "new")
                $SCRIPT/fill-space
                ;;
              "close")
                ;;
              "focus")
                ;;
              "resize")
                polybar-msg action menu hook 1
                ;;
              "apps")
                polybar-msg action menu hook 2
                ;;
              "default")
                polybar-msg action menu hook 0
                ;;
              *)
                continue
                ;;
            esac

            ${mouse-check}

            sleep ${toString (1.0 / 30.0)}

            tree=$(i3-msg -t get_tree)
            layout="$(echo $tree | jaq -r 'recurse(.nodes[];.nodes!=null)|select(.nodes[].focused)|"\(.layout)"')"

            eval "$(echo $tree | jaq -r 'recurse(.nodes[];.nodes!=null)|select(.nodes == [] and .focused)|"id=\(.id) width=\(.rect.width) height=\(.rect.height)"')"

            if [ $width -gt $height ] && [ $layout = "splitv" ]; then
              msg="split horizontal"
            elif [ $height -gt $width ] && [ $layout = "splith" ]; then
              msg="split vertical"
            else
              msg=""
            fi

            if [ "$msg" ]; then
              ${mouse-check}

              i3-msg "[con_id=\"$id\"] $msg" > /dev/null
            fi
          done
        '';
    };
    xsession.windowManager.i3 = {
      enable = true;
      config = {
        # #### ## #
        # THEMING
        # #### ## #
        fonts = {
          names = [ "Cascadia Mono" ];
          size = 10.0;
        };
        colors = {
          background = colors.blackDim;
          focused = {
            border = colors.foreground; # titlebar border
            background = colors.black; # titlebar background
            text = colors.foreground; # titlebar text
            childBorder = colors.blackBright; # window border
            indicator = colors.foreground; # window target border
          };
          focusedInactive = {
            border = colors.black; # titlebar border
            background = colors.blackDim; # titlebar background
            text = colors.black; # titlebar text
            childBorder = colors.blackDim; # window border
            indicator = colors.blackDim; # window target border
          };
          unfocused = {
            border = colors.blackDim; # titlebar border
            background = colors.blackDim; # titlebar background
            text = colors.black; # titlebar text
            childBorder = colors.blackDim; # window border
            indicator = colors.blackDim; # window target border
          };
          placeholder = {
            border = colors.blackDim; # titlebar border
            background = colors.blackDim; # titlebar background
            text = colors.blackDim; # titlebar text
            childBorder = colors.blackDim; # window border
            indicator = colors.blackDim; # window target border
          };
          urgent = {
            border = colors.yellowDim; # titlebar border
            background = colors.black; # titlebar background
            text = colors.yellow; # titlebar text
            childBorder = colors.blackBright; # window border
            indicator = colors.blackBright; # window target border
          };
        };
        gaps = {
          inner = 18;
          outer = 0;
          smartBorders = "on";
          smartGaps = true;
        };
        # #### ## #
        # BEHAVIOR
        # #### ## #
        modifier = "Mod4";
        defaultWorkspace = "1";
        workspaceAutoBackAndForth = false;
        focus = {
          followMouse = false;
          newWindow = "focus";
          wrapping = "workspace";
        };
        floating = {
          border = 3; # theming
          criteria = [
            { class = "Yad"; }
            { class = "copyq"; }
            { class = "qalculate-qt"; }
            { class = "pavucontrol-qt"; }
            { class = "bluedevil-wizard"; }
            { class = "pcmanfm-qt"; }
            { class = "TelegramDesktop"; }
            { class = "WebCord"; }
            { class = "Whatsapp-for-linux"; }
            { class = "qps"; }
          ];
          modifier = mod;
          titlebar = false;
        };
        window = {
          border = 1; # theming
          commands = [
            {
              command = "resize set 1280 px 960 px, move position center";
              criteria.class = "copyq";
            }
            {
              command = "resize set 768 px 640 px, move position center";
              criteria.class = "qalculate-qt";
            }
            {
              command = "resize set 1115 px 839 px, move position center";
              criteria.class = "pavucontrol-qt";
            }
            {
              command = "resize set 750 px 678 px, move position center";
              criteria.class = "bluedevil-wizard";
            }
            {
              command = "move position center";
              criteria.class = "pcmanfm-qt";
            }
            {
              command = "move position center";
              criteria.class = "TelegramDesktop";
            }
            {
              command = "move position center";
              criteria.class = "WebCord";
            }
            {
              command = "move position center";
              criteria.class = "Whatsapp-for-linux";
            }
            {
              command = "move position center";
              criteria.class = "qps";
            }
            { 
              command = "resize set 1280 px 720 px, move position center";
              criteria.class = "WebCord";
            }
          ];
          hideEdgeBorders = "smart";
          titlebar = false;
        };
        # #### ## #
        # SESSION
        # #### ## #
        startup = [
          {
            command = "$SCRIPT/daemon";
            always = true;
            notification = false;
          }
        ];
        # #### ## #
        # KEYBINDINGS
        # #### ## #
        keybindings = {
          ######## #### ## #
          # MISC
          ######## #### ## #

          # reload config
          "${mod}+Escape" = "reload";

          # restart wm
          "${mod}+Shift+Escape" = "restart";

          # quit sessions
          "Control+Mod1+Escape" = "exec i3-nagbar -t warning -m 'Do you want to exit i3?' -b 'Yes' 'i3-msg exit'";

          # open main menu
          "${mod}+Return" = "exec ${menu}";

          # open terminal
          "${mod}+BackSpace" = "exec --no-startup-id ${terminal}";
          "${mod}+semicolon" = "exec --no-startup-id ${terminal}";

          ######## #### ## #
          # WINDOW CONTROLS
          ######## #### ## #

          # ## #
          # primary actions (close/maximize)
          # ## #

          # [c] - close / kill
          "${mod}+c" = "kill";
          "${mod}+Shift+c" = "exec --no-startup-id xkill -id $(xdotool getwindowfocus)";

          # [f] - toggle float / toggle sticky float
          "${mod}+f" = "floating toggle";
          "${mod}+shift+f" = "floating enable; sticky toggle";

          # [m] - toggle maximize / minimize
          "${mod}+m" = "fullscreen toggle";
          "${mod}+shift+m" = "move scratchpad";

          # [r] - resize mode
          "${mod}+r" = "mode resize";

          # [a] - app launcher mode
          "${mod}+a" = "mode apps";

          # ## #
          # secondary actions (layout/pseudo float/fix rotate brother)
          # ## #

          # [s] - toggle split layout
          "${mod}+s" = script "toggle-split";
          "${mod}+shift+s" = script "select-split";

          # [t] - toggle split / tabbed layouts  
          "${mod}+t" = script "toggle-tabs";

          # toggle floating focus
          "${mod}+space" = "focus mode_toggle; ${script "cursor-warp"}";

          # floating windows alt tab
          "${alt}+Tab" = "focus prev";
          "${alt}+shift+Tab" = "focus next";

          # ## #
          # tertiary actions (show desktop/equalize/balance)
          # ## #

          # [g] - toggle gaps
          "${mod}+g" = "gaps inner all toggle ${inner}; gaps outer all toggle ${outer}";

          # [i] - insert space / fill space
          "${mod}+i" = script "make-space";
          "${mod}+shift+i" = script "fill-space";

          # [v] - scratchpad show
          "${mod}+v" = "scratchpad show";

          # tabbed windows super tab
          "${mod}+Tab" = "focus prev sibling";
          "${mod}+shift+Tab" = "focus next sibling";

          ######## #### ## #
          # FOCUS AND MOVEMENT
          ######## #### ## #

          # focus with vim keys
          "${mod}+h" = focus "left";
          "${mod}+j" = focus "down";
          "${mod}+k" = focus "up";
          "${mod}+l" = focus "right";

          # focus with arrow keys
          "${mod}+Left" = focus "left";
          "${mod}+Down" = focus "down";
          "${mod}+Up" = focus "up";
          "${mod}+Right" = focus "right";

          # move with vim keys
          "${mod}+Shift+h" = move "left";
          "${mod}+Shift+j" = move "down";
          "${mod}+Shift+k" = move "up";
          "${mod}+Shift+l" = move "right";

          # move with arrow keys
          "${mod}+Shift+Left" = move "left";
          "${mod}+Shift+Down" = move "down";
          "${mod}+Shift+Up" = move "up";
          "${mod}+Shift+Right" = move "right";

          # [p] - toggle focus parent / child container
          "${mod}+p" = "focus parent";
          "${mod}+shift+p" = "focus child";

          ######## #### ## #
          # WORKSPACES
          ######## #### ## #

          "${mod}+apostrophe" = workspace 0;
          "${mod}+1" = workspace 1;
          "${mod}+2" = workspace 2;
          "${mod}+3" = workspace 3;
          "${mod}+4" = workspace 4;
          "${mod}+5" = workspace 5;
          "${mod}+6" = workspace 6;
          "${mod}+7" = workspace 7;
          "${mod}+8" = workspace 8;
          "${mod}+9" = workspace 9;
          "${mod}+0" = workspace 10;

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
        };
        modes = {
          apps = {
            "Escape" = "mode default";
            "Return" = "mode default";
            "a" = "mode default";
          };
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
        # #### ## #
        # MISC
        # #### ## #
        bars = [ ];
      };
      extraConfig =
        ''
          title_align center
        '';
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
