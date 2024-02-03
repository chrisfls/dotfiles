{ config, lib, pkgs, ... }:
let
  inherit (config.presets) archlinux;
  inherit (config.modules.i3wm)
    enable
    apps
    startup
    extraConfig
    window-list
    menu
    power-menu
    run
    screenshot
    terminal;
  inherit (builtins) concatStringsSep;

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

  black = "#000000";
  silver = "#C0C0C0";
  gray = "#808080";
  dark-gray = "#404040";

  i3 =
    if archlinux then "/usr/bin/i3"
    else "${pkgs.i3}/bin/i3";

  i3-msg =
    if archlinux then "/usr/bin/i3-msg"
    else "${pkgs.i3}/bin/i3-msg";
in
{
  options.modules.i3wm = {
    enable = lib.mkEnableOption "Enable i3wm module";

    terminal = lib.mkOption { type = lib.types.str; };
    menu = lib.mkOption { type = lib.types.str; default = "rofi-menu"; };
    run = lib.mkOption { type = lib.types.str; default = "rofi-run"; };
    power-menu = lib.mkOption { type = lib.types.str; default = "rofi-power-menu"; };
    window-list = lib.mkOption { type = lib.types.str; default = "rofi-windows"; };

    screenshot = {
      copy = lib.mkOption { type = lib.types.str; default = "screenshot-copy"; };
      copy-area = lib.mkOption { type = lib.types.str; default = "screenshot-copy-area"; };
      save = lib.mkOption { type = lib.types.str; default = "screenshot-save"; };
    };

    apps = lib.mkOption { type = lib.types.attrsOf lib.types.str; default = { }; };
    startup = lib.mkOption { type = lib.types.listOf lib.types.str; default = [ ]; };

    extraConfig = lib.mkOption { type = lib.types.lines; default = ""; };
  };

  config = lib.mkIf enable {
    home.packages = lib.mkIf (!archlinux) [ pkgs.i3 ];

    pacman.packages = [ "extra/i3-wm" ];

    modules.xorg.window-manager = i3;

    xdg.configFile."i3/config" = {
      text =
        ''
          # ######## #### ## #
          # THEMING
          # ######## #### ## #

          font pango:Noto Sans Mono Bold 10

          client.background ${black}
          client.focused ${colors.foreground} ${colors.black} ${colors.foreground} ${silver} ${gray}
          client.focused_inactive ${colors.black} ${colors.background} ${colors.foreground} ${dark-gray} ${dark-gray}
          client.placeholder ${black} ${black} ${black} ${black} ${black}
          client.unfocused ${black} ${colors.background} ${colors.black} ${black} ${black}
          client.urgent ${black} ${colors.background} ${colors.yellow} ${black} ${black}

          default_border normal 1
          default_floating_border normal 1
          gaps inner 0
          gaps outer 0
          hide_edge_borders none
          hide_edge_borders smart_no_gaps
          smart_borders on
          title_align left
          workspace_auto_back_and_forth no
        
          # ######## #### ## #
          # BEHAVIOR
          # ######## #### ## #

          default_orientation horizontal
          floating_modifier Mod4
          focus_follows_mouse no
          focus_on_window_activation focus
          focus_wrapping workspace
          mouse_warping output
          tiling_drag modifier titlebar
          workspace_layout default
        
          for_window [class="bluedevil-wizard"] floating enable
          for_window [class="bluedevil-wizard"] resize set 750 px 678 px, move position center
          for_window [class="copyq"] floating enable
          for_window [class="copyq"] resize set 1280 px 960 px, move position center
          for_window [class="Logseq"] floating enable
          for_window [class="pavucontrol-qt"] floating enable
          for_window [class="pavucontrol-qt"] resize set 1115 px 839 px, move position center
          for_window [class="pcmanfm-qt"] floating enable
          for_window [class="pcmanfm-qt"] move position center
          for_window [class="qalculate-qt"] floating enable
          for_window [class="qalculate-qt"] resize set 768 px 640 px, move position center
          for_window [class="qps"] floating enable
          for_window [class="qps"] move position center
          for_window [class="TelegramDesktop"] floating enable
          for_window [class="TelegramDesktop"] move position center
          for_window [class="WebCord"] floating enable
          for_window [class="WebCord"] move position center
          for_window [class="WebCord"] resize set 1280 px 720 px, move position center
          for_window [class="Whatsapp-for-linux"] floating enable
          for_window [class="Whatsapp-for-linux"] move position center
          for_window [class="Yad"] floating enable

          ${
            lib.trivial.pipe startup [
              (builtins.map (cmd: "exec --no-startup-id ${cmd}"))
              (builtins.concatStringsSep "\n")
            ]
          }

          exec_always --no-startup-id ${daemon}

          # ######## #### ## #
          # KEYBINDINGS
          # ######## #### ## #

          set $mod Mod4
          set $alt Mod1

          # #### ## #
          # MISC
          # #### ## #

          # reload config
          bindsym $mod+Escape reload

          # restart wm
          bindsym $mod+shift+Escape restart
        
          # quit sessions
          bindsym Control+$alt+Escape exec i3-nagbar -t warning -m 'Do you want to exit i3?' -b 'Yes' 'i3-msg exit'

          # open main menu
          bindsym $mod+Return exec --no-startup-id "${menu}"
          bindsym $mod+shift+Return exec --no-startup-id "${run}"

          # open terminal
          bindsym $mod+BackSpace exec --no-startup-id "${terminal}"
          bindsym $mod+semicolon exec --no-startup-id "${terminal}"

          # open rofi calculator
          bindsym $mod+shift+BackSpace exec --no-startup-id "rofi-calc"
          bindsym $mod+shift+semicolon exec --no-startup-id "rofi-calc"

          # power menu
          bindsym $mod+q exec --no-startup-id "${power-menu}"

          # list windows
          bindsym $mod+w exec --no-startup-id "${window-list}"

          # screenshot
          bindsym Print exec --no-startup-id "${screenshot.copy}"
          bindsym shift+Print exec --no-startup-id "${screenshot.save}"
          bindsym $mod+Print exec --no-startup-id "${screenshot.copy-area}"
        
          # #### ## #
          # WINDOW CONTROLS
          # #### ## #

          # ## #
          # primary actions (close/maximize)
          # ## #

          # [c] - close / kill
          bindsym $mod+c kill
          bindsym $mod+shift+c exec --no-startup-id xkill -id $(xdotool getwindowfocus)

          # [f] - toggle float / toggle sticky float
          bindsym $mod+f floating toggle
          bindsym $mod+shift+f floating enable; sticky toggle

          # [m] - toggle maximize
          bindsym $mod+m fullscreen toggle

          # [r] - resize mode
          bindsym $mod+r mode resize

          # [a] - app launcher mode
          bindsym $mod+a mode apps

          # ## #
          # secondary actions (layout/pseudo float/fix rotate brother)
          # ## #
        
          # [s] - toggle split layout
          bindsym $mod+s exec --no-startup-id ${presel-split}
          bindsym $mod+shift+s exec --no-startup-id ${toggle-split}

          # [t] - toggle split / tabbed layouts  
          bindsym $mod+t exec --no-startup-id ${toggle-tabs}

          # toggle floating focus
          bindsym $mod+space focus mode_toggle; exec --no-startup-id ${cursor-wrap}

          # floating windows alt tab
          bindsym $alt+Tab focus prev
          bindsym $alt+shift+Tab focus next

          # ## #
          # tertiary actions (show desktop/equalize/balance)
          # ## #

          # [g] - toggle gaps
          bindsym $mod+g gaps inner current toggle 18; gaps outer current toggle 0
          bindsym $mod+shift+g gaps inner current toggle 64; gaps outer current toggle 64

          # [i] - insert space / fill space
          bindsym $mod+i [tiling con_id="__focused__"] exec --no-startup-id ${insert}
          bindsym $mod+shift+i move container to mark insert; unmark insert

          # [v] - scratchpad show
          bindsym $mod+v scratchpad show
          bindsym $mod+shift+v move scratchpad

          # tabbed windows super tab
          bindsym $mod+Tab focus prev sibling
          bindsym $mod+shift+Tab focus next sibling

          # #### ## #
          # FOCUS AND MOVEMENT
          # #### ## #

          # focus with vim keys
          bindsym $mod+h focus left; exec --no-startup-id ${cursor-wrap}
          bindsym $mod+j focus down; exec --no-startup-id ${cursor-wrap}
          bindsym $mod+k focus up; exec --no-startup-id ${cursor-wrap}
          bindsym $mod+l focus right; exec --no-startup-id ${cursor-wrap}
        
          # focus with arrow keys
          bindsym $mod+Left focus left; exec --no-startup-id ${cursor-wrap}
          bindsym $mod+Down focus down; exec --no-startup-id ${cursor-wrap}
          bindsym $mod+Up focus up; exec --no-startup-id ${cursor-wrap}
          bindsym $mod+Right focus right; exec --no-startup-id ${cursor-wrap}

          # swap with vim keys
          bindsym $mod+shift+h mark swap; focus left; swap container with mark swap; [con_mark="swap"] focus; unmark swap; exec --no-startup-id ${cursor-wrap}
          bindsym $mod+shift+j mark swap; focus down; swap container with mark swap; [con_mark="swap"] focus; unmark swap; exec --no-startup-id ${cursor-wrap}
          bindsym $mod+shift+k mark swap; focus up; swap container with mark swap; [con_mark="swap"] focus; unmark swap; exec --no-startup-id ${cursor-wrap}
          bindsym $mod+shift+l mark swap; focus right; swap container with mark swap; [con_mark="swap"] focus; unmark swap; exec --no-startup-id ${cursor-wrap}
        
          # swap with arrow keys
          bindsym $mod+shift+Left mark swap; focus left; swap container with mark swap; [con_mark="swap"] focus; unmark swap; exec --no-startup-id ${cursor-wrap}
          bindsym $mod+shift+Down mark swap; focus down; swap container with mark swap; [con_mark="swap"] focus; unmark swap; exec --no-startup-id ${cursor-wrap}
          bindsym $mod+shift+Up mark swap; focus up; swap container with mark swap; [con_mark="swap"] focus; unmark swap; exec --no-startup-id ${cursor-wrap}
          bindsym $mod+shift+Right mark swap; focus right; swap container with mark swap; [con_mark="swap"] focus; unmark swap; exec --no-startup-id ${cursor-wrap}

          # move with vim keys
          bindsym $mod+$alt+h move left; exec --no-startup-id ${cursor-wrap}
          bindsym $mod+$alt+j move down; exec --no-startup-id ${cursor-wrap}
          bindsym $mod+$alt+k move up; exec --no-startup-id ${cursor-wrap}
          bindsym $mod+$alt+l move right; exec --no-startup-id ${cursor-wrap}

          # move with arrow keys keys; exec --no-startup-id ${cursor-wrap}
          bindsym $mod+$alt+Left move left; exec --no-startup-id ${cursor-wrap}
          bindsym $mod+$alt+Down move down; exec --no-startup-id ${cursor-wrap}
          bindsym $mod+$alt+Up move up; exec --no-startup-id ${cursor-wrap}
          bindsym $mod+$alt+Right move right; exec --no-startup-id ${cursor-wrap}

          # [p] - toggle focus parent / child container
          bindsym $mod+p focus parent
          bindsym $mod+shift+p focus child

          # #### ## #
          # WORKSPACES
          # #### ## #

          bindsym $mod+shift+apostrophe move container to workspace number 0
          bindsym $mod+1 workspace number 1; exec --no-startup-id ${cursor-wrap}
          bindsym $mod+2 workspace number 2; exec --no-startup-id ${cursor-wrap}
          bindsym $mod+3 workspace number 3; exec --no-startup-id ${cursor-wrap}
          bindsym $mod+4 workspace number 4; exec --no-startup-id ${cursor-wrap}
          bindsym $mod+5 workspace number 5; exec --no-startup-id ${cursor-wrap}
          bindsym $mod+6 workspace number 6; exec --no-startup-id ${cursor-wrap}
          bindsym $mod+7 workspace number 7; exec --no-startup-id ${cursor-wrap}
          bindsym $mod+8 workspace number 8; exec --no-startup-id ${cursor-wrap}
          bindsym $mod+9 workspace number 9; exec --no-startup-id ${cursor-wrap}
          bindsym $mod+0 workspace number 10; exec --no-startup-id ${cursor-wrap}
        
          bindsym $mod+apostrophe workspace number 0; exec --no-startup-id ${cursor-wrap}
          bindsym $mod+shift+1 move container to workspace number 1
          bindsym $mod+shift+2 move container to workspace number 2
          bindsym $mod+shift+3 move container to workspace number 3
          bindsym $mod+shift+4 move container to workspace number 4
          bindsym $mod+shift+5 move container to workspace number 5
          bindsym $mod+shift+6 move container to workspace number 6
          bindsym $mod+shift+7 move container to workspace number 7
          bindsym $mod+shift+8 move container to workspace number 8
          bindsym $mod+shift+9 move container to workspace number 9
          bindsym $mod+shift+0 move container to workspace number 10

          mode "resize" {
            bindsym h resize shrink width 50 px or 5 ppt
            bindsym j resize grow height 50 px or 5 ppt
            bindsym k resize shrink height 50 px or 5 ppt
            bindsym l resize grow width 50 px or 5 ppt

            bindsym Left resize shrink width 50 px or 5 ppt
            bindsym Down resize grow height 50 px or 5 ppt
            bindsym Up resize shrink height 50 px or 5 ppt
            bindsym Right resize grow width 50 px or 5 ppt

            bindsym Escape mode default
            bindsym Return mode default
            bindsym r mode default
          }

          mode "apps" {
            ${
              lib.trivial.pipe apps [
                (lib.attrsets.mapAttrsToList
                  (name: app-name:
                    "bindsym ${name} exec --no-startup-id gtk-launch ${app-name}; mode default"))
                  (builtins.concatStringsSep "\n  ")
              ]
            }

            bindsym Escape mode default
            bindsym Return mode default
            bindsym a mode default
          }

          ${extraConfig}
        '';
      onChange = ''
        # There may be several sockets after log out/log in, but the old ones
        # will fail with "Connection refused".
        for i3Socket in ''${XDG_RUNTIME_DIR:-/run/user/$UID}/i3/ipc-socket.*; do
          if [[ -S $i3Socket ]]; then
            ${i3-msg} -s $i3Socket reload >/dev/null |& grep -v "Connection refused" || true
          fi
        done
      '';
    };
  };
}
