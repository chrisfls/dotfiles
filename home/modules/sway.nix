{ config, lib, pkgs, ... }:
let
  inherit (config.modules.sway)
    enable
    apps
    startup
    extraConfig
    window-list
    menu
    power-menu
    run
    screenshot
    terminal
    calculator;
  inherit (builtins) concatStringsSep;

  focus = dir:
    "focus ${dir}";

  move = dir:
    "mark swap; focus ${dir}; swap container with mark swap; [con_mark=\"swap\"] focus; unmark swap";

  workspace = num:
    "workspace number ${toString num}";

  black = "#31363B"; # background
  white = "#FCFCFC"; # foreground

  prim0 = "#3DAEE9"; # border (unused)
  prim1 = "#508BAA"; # border
  prim2 = "#355E75"; # background / indicator
  prim3 = "#334E5E"; # background

  gray0 = "#6e7173"; # text
  gray1 = "#64686B"; # border
  gray2 = "#404448"; # border
  gray3 = "#2f3338"; # background

  acct0 = "#a87656"; # border
  acct1 = "#5c4434"; # background

  sway = "/usr/bin/sway";
  swaynag = "/usr/bin/swaynag";
  swaymsg = "/usr/bin/swaymsg";
in
{
  options.modules.sway = {
    enable = lib.mkEnableOption "Enable sway module";

    terminal = lib.mkOption { type = lib.types.str; };
    menu = lib.mkOption { type = lib.types.str; default = "rofi-menu"; };
    run = lib.mkOption { type = lib.types.str; default = "rofi-run"; };
    power-menu = lib.mkOption { type = lib.types.str; default = "rofi-power-menu"; };
    window-list = lib.mkOption { type = lib.types.str; default = "rofi-windows"; };
    calculator = lib.mkOption { type = lib.types.str; default = "rofi-calc"; };

    screenshot = {
      copy = lib.mkOption { type = lib.types.str; default = "screenshot-copy"; };
      copy-area = lib.mkOption { type = lib.types.str; default = "screenshot-copy-area"; };
      save = lib.mkOption { type = lib.types.str; default = "screenshot-save"; };
    };

    apps = lib.mkOption { type = lib.types.attrsOf lib.types.str; default = { }; };
    startup = lib.mkOption { type = lib.types.listOf lib.types.str; default = [ ]; };

    extraConfig = lib.mkOption { type = lib.types.lines; default = ""; };
  };

  #  

  config = lib.mkIf enable {
    pacman.packages = [
      "extra/swaybg"
      "chaotic-aur/swayfx"
    ];

    modules.systemd.imported-variables = [ "WAYLAND_DISPLAY" ];

    xdg.configFile."sway/config" = {
      text =
        ''
          # ######## #### ## #
          # THEMING
          # ######## #### ## #

          font pango:Noto Sans Mono Bold 11

          #                       border       bg           txt          indicator    child_border
          #                       ------------ ------------ ------------ ------------ ------------ 
          client.background       ${black}
          client.focused          ${prim1}     ${prim3}     ${white}     ${prim2}     ${gray1}
          client.urgent           ${acct0}     ${acct1}     ${white}     ${black}     ${gray2}
          client.focused_inactive ${gray1}     ${black}     ${white}     ${black}     ${gray2}

          client.unfocused        ${black}     ${black}     ${gray0}     ${black}     ${black}
          client.placeholder      ${black}     ${black}     ${gray0}     ${black}     ${black}

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
          for_window [class="dolphin"] floating enable
          for_window [class="dolphin"] move position center
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
          bindsym Control+$alt+Escape exec ${swaynag} -t warning -m 'Do you want to exit sway?' -b 'Yes' '${swaymsg} exit'

          # open main menu
          bindsym $mod+Return exec --no-startup-id "${menu}"
          bindsym $mod+shift+Return exec --no-startup-id "${run}"

          # open terminal
          bindsym $mod+BackSpace exec --no-startup-id "${terminal}"
          bindsym $mod+semicolon exec --no-startup-id "${terminal}"

          # open rofi calculator
          bindsym $mod+shift+BackSpace exec --no-startup-id "${calculator}"
          bindsym $mod+shift+semicolon exec --no-startup-id "${calculator}"

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
          bindsym $mod+s split toggle
          bindsym $mod+shift+s layout toggle splitv splith

          # [t] - toggle split / tabbed layouts  
          bindsym $mod+t layout toggle tabbed split

          # toggle floating focus
          bindsym $mod+space focus mode_toggle

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
          bindsym $mod+i [tiling con_id="__focused__"] mark --toggle insert
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
          bindsym $mod+h focus left
          bindsym $mod+j focus down
          bindsym $mod+k focus up
          bindsym $mod+l focus right
        
          # focus with arrow keys
          bindsym $mod+Left focus left
          bindsym $mod+Down focus down
          bindsym $mod+Up focus up
          bindsym $mod+Right focus right

          # swap with vim keys
          bindsym $mod+shift+h mark swap; focus left; swap container with mark swap; [con_mark="swap"] focus; unmark swap
          bindsym $mod+shift+j mark swap; focus down; swap container with mark swap; [con_mark="swap"] focus; unmark swap
          bindsym $mod+shift+k mark swap; focus up; swap container with mark swap; [con_mark="swap"] focus; unmark swap
          bindsym $mod+shift+l mark swap; focus right; swap container with mark swap; [con_mark="swap"] focus; unmark swap
        
          # swap with arrow keys
          bindsym $mod+shift+Left mark swap; focus left; swap container with mark swap; [con_mark="swap"] focus; unmark swap
          bindsym $mod+shift+Down mark swap; focus down; swap container with mark swap; [con_mark="swap"] focus; unmark swap
          bindsym $mod+shift+Up mark swap; focus up; swap container with mark swap; [con_mark="swap"] focus; unmark swap
          bindsym $mod+shift+Right mark swap; focus right; swap container with mark swap; [con_mark="swap"] focus; unmark swap

          # move with vim keys
          bindsym $mod+$alt+h move left
          bindsym $mod+$alt+j move down
          bindsym $mod+$alt+k move up
          bindsym $mod+$alt+l move right

          # move with arrow keys keys
          bindsym $mod+$alt+Left move left
          bindsym $mod+$alt+Down move down
          bindsym $mod+$alt+Up move up
          bindsym $mod+$alt+Right move right

          # [p] - toggle focus parent / child container
          bindsym $mod+p focus parent
          bindsym $mod+shift+p focus child

          # #### ## #
          # WORKSPACES
          # #### ## #

          bindsym $mod+shift+apostrophe move container to workspace number 0
          bindsym $mod+1 workspace number 1
          bindsym $mod+2 workspace number 2
          bindsym $mod+3 workspace number 3
          bindsym $mod+4 workspace number 4
          bindsym $mod+5 workspace number 5
          bindsym $mod+6 workspace number 6
          bindsym $mod+7 workspace number 7
          bindsym $mod+8 workspace number 8
          bindsym $mod+9 workspace number 9
          bindsym $mod+0 workspace number 10
        
          bindsym $mod+apostrophe workspace number 0
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

          input type:keyboard {
              xkb_layout br
          }

          blur enable
          shadows enable

          ${extraConfig}
        '';
      onChange = ''
        # There may be several sockets after log out/log in, but the old ones
        # will fail with "Connection refused".
        for swaySocket in ''${XDG_RUNTIME_DIR:-/run/user/$UID}/sway/ipc-socket.*; do
          if [[ -S $swaySocket ]]; then
            ${swaymsg} -s $swaySocket reload >/dev/null |& grep -v "Connection refused" || true
          fi
        done
      '';
    };
  };
}
