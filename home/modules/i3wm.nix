{ config, lib, pkgs, ... }:
let
  inherit (config.modules.i3wm) enable;
  inherit (builtins) concatStringsSep;
  inherit (config.xsession.windowManager.i3.config) terminal menu;

  mod = config.xsession.windowManager.i3.config.modifier;
  alt = "Mod1";
  colors = config.modules.themes.color-scheme;

  focus = dir:
    "focus ${dir}; exec --no-startup-id ${cursor-wrap}";

  move = dir:
    "mark swap; focus ${dir}; swap container with mark swap; [con_mark=\"swap\"] focus; unmark swap; exec --no-startup-id ${cursor-wrap}";

  workspace = num:
    "workspace number ${toString num}; exec --no-startup-id ${cursor-wrap}";

  /*

    # fetch win_id, self_con, parent_con

    $ eval "$(i3-msg -t get_tree | jaq -r 'recurse(.nodes[];.nodes!=null)|select(.nodes[].nodes[].focused)|"id=\(.nodes[].nodes[]|select(.focused).id) self=\(.nodes[]|select(.nodes[].focused).layout) parent=\(select(.nodes[].nodes[].focused).layout)"')"

    # fetch win_id, self_con, parent_con, output_con

    $ eval "$(i3-msg -t get_tree | jaq -r 'recurse(.nodes[];.nodes!=null)|select(.nodes[].nodes[].nodes[].focused)|"id=\(.nodes[].nodes[].nodes[]|select(.focused).id) self=\(.nodes[].nodes[]|select(.nodes[].focused).layout) parent=\(.nodes[]|select(.nodes[].nodes[].focused).layout) output=\(select(.nodes[].nodes[].nodes[].focused).layout)"')"

  */

  cursor-wrap = pkgs.writeScript "i3-cursor-wrap"
    # move mouse to center of the window
    ''
      eval $(xdotool getwindowfocus getwindowgeometry --shell)
      xdotool mousemove $((X + (WIDTH / 2))) $((Y + (HEIGHT / 2)))
    '';

  insert = pkgs.writeScript "i3-insert"
    # select insertion spot
    ''
      while i3-msg "focus child"; do
        :
      done

      i3-msg "[tiling con_id=\"__focused__\"] mark --toggle insert"
    '';

  pop = pkgs.writeScript "i3-pop"
    # remove window from tab container (REVIEW: still not quite working on vertical setups)
    ''
      while i3-msg "focus child"; do
        :
      done

      eval "$(i3-msg -t get_tree | jaq -r 'recurse(.nodes[];.nodes!=null)|select(.nodes[].nodes[].nodes[].focused)|"id=\(.nodes[].nodes[].nodes[]|select(.focused).id) self=\(.nodes[].nodes[]|select(.nodes[].focused).layout) parent=\(select(.nodes[].nodes[].nodes[].focused).layout)"')"

      if [ "$self" = "tabbed" ]; then
        if [ "$parent" = "splitv" ]; then
          i3-msg "focus parent; split vertical; focus parent; mark swap; focus child; focus child; move container to mark swap; unmark swap"
        elif [ "$parent" = "splith" ]; then
          i3-msg "focus parent; split horizontal; focus parent; mark swap; focus child; focus child; move container to mark swap; unmark swap"
        fi
      fi
    '';

  pop-shift = pkgs.writeScript "i3-pop-shift"
    # remove window from tab container in opposite orientation
    ''
      while i3-msg "focus child"; do
        :
      done

      eval "$(i3-msg -t get_tree | jaq -r 'recurse(.nodes[];.nodes!=null)|select(.nodes[].nodes[].nodes[].focused)|"id=\(.nodes[].nodes[].nodes[]|select(.focused).id) self=\(.nodes[].nodes[]|select(.nodes[].focused).layout) parent=\(select(.nodes[].nodes[].nodes[].focused).layout)"')"

      if [ "$self" = "tabbed" ]; then
        if [ "$parent" = "splitv" ]; then
          i3-msg "focus parent; split horizontal; focus parent; mark swap; focus child; focus child; move container to mark swap; unmark swap"
        elif [ "$parent" = "splith" ]; then
          i3-msg "focus parent; split vertical; focus parent; mark swap; focus child; focus child; move container to mark swap; unmark swap"
        fi
      fi
    '';

  presel-split = pkgs.writeScript "i3-presel-split"
    # preselect next split orientation
    ''
      while i3-msg "focus child"; do
        :
      done

      eval "$(i3-msg -t get_tree | jaq -r 'recurse(.nodes[];.nodes!=null)|select(.layout!="tabbed" and .nodes[].nodes[].focused)|"id=\((.nodes[].nodes[]|select(.focused)|.id)) tabbed=\((.nodes[]|select(.nodes[].focused)|.layout))"')"

      if [ "$tabbed" != "tabbed" ]; then
        i3-msg "[con_id=\"$id\"] split toggle"
      fi
    '';

  toggle-split = pkgs.writeScript "i3-toggle-split"
    # toggle split orientation
    ''
      id=$(i3-msg -t get_tree | jaq -r 'recurse(.nodes[];.nodes!=null)|select(.nodes[].focused or .focused).id')

      if [ "$id" ]; then
        i3-msg "[con_id=\"$id\"] layout toggle splitv splith"
      fi
    '';

  toggle-tabs = pkgs.writeScript "i3-toggle-tabs"
    # toggle tabbed layout at the selected leaf
    ''
      id=$(i3-msg -t get_tree | jaq -r 'recurse(.nodes[];.nodes!=null)|select(.nodes == [] and .focused).id')

      if [ "$id" ]; then
        i3-msg "[con_id=\"$id\"] layout toggle tabbed split"
      fi
    '';

  daemon = pkgs.writeScript "i3-daemon"
    # auto alternate splits and move new windows to insert spaces
    ''
      mouse=$(xinput --list | grep -i -m 1 'Logitech USB Optical Mouse' | grep -o 'id=[0-9]\+' | grep -o '[0-9]\+')

      i3-msg -t subscribe -m '[ "window", "mode" ]' | while IFS= read -r line; do
        event=$(echo $line | jaq -r '.change')

        case "$event" in
          "new")
            con_id=$(echo $line | jaq -r '.container.id')
            i3-msg "[con_id=\"$con_id\"]move container to mark insert; unmark insert" && continue
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
        esac
      done
    '';
in
{
  options.modules.i3wm.enable = lib.mkEnableOption "Enable i3wm module";

  config = lib.mkIf enable {
    home.packages = [ pkgs.cozette ];

    xsession.windowManager.i3 = {
      enable = true;
      config = {
        # #### ## #
        # THEMING
        # #### ## #
        fonts = {
          names = [ "cozette" ];
          size = 8.0; # 5.33333333333;
          # names = [ "Cascadia Mono" ];
          # size = 7.666666666665;
        };
        colors = {
          background = "#000000";
          focused = {
            border = colors.foreground; # titlebar border
            background = colors.black; # titlebar background
            text = colors.foreground; # titlebar text
            childBorder = "#808080"; # window border
            indicator = "#C0C0C0"; # window target border
          };
          focusedInactive = {
            border = colors.black; # titlebar border
            background = colors.background; # titlebar background
            text = colors.foreground; # titlebar text
            childBorder = "#404040"; # window border
            indicator = "#404040"; # window target border
          };
          unfocused = {
            border = "#000000"; # titlebar border
            background = colors.background; # titlebar background
            text = colors.black; # titlebar text
            childBorder = "#000000"; # window border
            indicator = "#000000"; # window target border
          };
          urgent = {
            border = "#000000"; # titlebar border
            background = colors.background; # titlebar background
            text = colors.yellow; # titlebar text
            childBorder = "#000000"; # window border
            indicator = "#000000"; # window target border
          };
          placeholder = {
            border = "#000000"; # titlebar border
            background = "#000000"; # titlebar background
            text = "#000000"; # titlebar text
            childBorder = "#000000"; # window border
            indicator = "#000000"; # window target border
          };
        };
        gaps = {
          inner = 0;
          outer = 0;
          smartBorders = "on"; # no_gaps
          smartGaps = false;
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
          border = 1;
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
            { class = "Logseq"; }
          ];
          modifier = mod;
          titlebar = true;
        };
        window = {
          border = 1;
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
          # hideEdgeBorders = "smart"; # smart_no_gaps
          titlebar = true;
        };
        # #### ## #
        # SESSION
        # #### ## #
        startup = [
          {
            command = "${daemon}";
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
          "${mod}+Return" = menu;

          # open terminal
          "${mod}+BackSpace" = terminal;
          "${mod}+semicolon" = terminal;

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

          # [m] - toggle maximize
          "${mod}+m" = "fullscreen toggle";

          # [r] - resize mode
          "${mod}+r" = "mode resize";

          # [a] - app launcher mode
          "${mod}+a" = "mode apps";

          # ## #
          # secondary actions (layout/pseudo float/fix rotate brother)
          # ## #

          # [s] - toggle split layout
          "${mod}+s" = "exec --no-startup-id ${presel-split}";
          "${mod}+shift+s" = "exec --no-startup-id ${toggle-split}";

          # [t] - toggle split / tabbed layouts  
          "${mod}+t" = "exec --no-startup-id ${toggle-tabs}";

          # toggle floating focus
          "${mod}+space" = "focus mode_toggle; exec --no-startup-id ${cursor-wrap}";

          # floating windows alt tab
          "${alt}+Tab" = "focus prev";
          "${alt}+shift+Tab" = "focus next";

          # ## #
          # tertiary actions (show desktop/equalize/balance)
          # ## #

          # [g] - toggle gaps
          "${mod}+g" = "gaps inner current toggle 18; gaps outer current toggle 0";
          "${mod}+shift+g" = "gaps inner current toggle 64; gaps outer current toggle 64";

          # [i] - insert space / fill space
          "${mod}+i" = "[tiling con_id=\"__focused__\"] exec --no-startup-id ${insert}";
          "${mod}+shift+i" = "move container to mark insert; unmark insert";

          "${mod}+o" = "[tiling con_id=\"__focused__\"] exec --no-startup-id ${pop}";
          "${mod}+shift+o" = "[tiling con_id=\"__focused__\"] exec --no-startup-id ${pop-shift}";

          # [v] - scratchpad show
          "${mod}+v" = "scratchpad show";
          "${mod}+shift+v" = "move scratchpad";

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
            "h" = "resize shrink width 50 px or 5 ppt";
            "j" = "resize grow height 50 px or 5 ppt";
            "k" = "resize shrink height 50 px or 5 ppt";
            "l" = "resize grow width 50 px or 5 ppt";

            "Left" = "resize shrink width 50 px or 5 ppt";
            "Down" = "resize grow height 50 px or 5 ppt";
            "Up" = "resize shrink height 50 px or 5 ppt";
            "Right" = "resize grow width 50 px or 5 ppt";

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
          title_align left
          default_orientation horizontal
          hide_edge_borders smart_no_gaps
          tiling_drag modifier titlebar
        '';
    };
  };
}
