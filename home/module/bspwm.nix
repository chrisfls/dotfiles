{ config, lib, pkgs, ... }:
let
  inherit (builtins) throw toString;
  inherit (lib) types;

  cfg = config.module.bspwm;

  colors = config.module.themes.color-scheme;

  dollar = "$";

  amount = toString cfg.amount;

  escape = "${pkgs.xdotool}/bin/xdotool key Escape";

  focus = direction:
    ''
      bspc node 'focused.!floating' --focus '${direction}.!floating.window'
      ||
      bspc node 'focused.floating' --focus '${direction}.floating.window'
    '';

  move = direction:
    let
      prefix = "bspc node 'focused.!floating.window' --swap ${direction}";
    in
    if direction == "west" then
      "${prefix} || bspc node 'focused.floating.window' --move -${amount} 0"
    else if direction == "south" then
      "${prefix} || bspc node 'focused.floating.window' --move 0 ${amount}"

    else if direction == "north" then
      "${prefix} || bspc node 'focused.floating.window' --move 0 -${amount}"

    else if direction == "east" then
      "${prefix} || bspc node 'focused.floating.window' --move ${amount} 0"

    else
      throw "Invalid move direction";

  lock =
    "flock --verbose -n $BSPWM_SCRATCHPAD/singleton.lock";

  # TODO: someday port these commands to dash scripts

  switch = 
    pkgs.writeShellScriptBin "bspwm-switch"
      ''
        bspc wm -h off;

        window=$(bspc query --nodes --node 'focused.local.!hidden.!floating.window');

        if [ "$window" ]; then
          modifier="!floating";
        else
          modifier="floating";
        fi;

        bspc node "$1.local.!hidden.$modifier.window" --focus || (
          if [ -z "$window" ]; then
            window=$(bspc query --nodes --node);
          fi;

          while true; do
            result=$(bspc query --nodes --node "$window#$2.local.!hidden.$modifier.window");

            if [ -z "$result" ]; then
              break;
            else
              window="$result";
            fi;
          done;

          bspc node $window --focus
        );

        bspc wm -h on
      '';

  minimize =
    pkgs.writeShellScriptBin "bspwm-minimize"
      ''
        node=$(bspc query --nodes --node);
        if [ -z "$node" ]; then exit 1; fi;

        desktop=$(bspc query --desktops --desktop);
        if [ -z "$desktop" ]; then exit 1; fi;

        store="$BSPWM_SCRATCHPAD/$desktop";
        if [ ! -f "$store" ]; then touch "$store"; fi;

        echo "$node" >> "$store";
        bspc node "$node" --flag hidden=on
      '';

  restore =
    pkgs.writeShellScriptBin "bspwm-restore"
      ''
        desktop=$(bspc query --desktops --desktop);
        if [ -z "$desktop" ]; then exit 1; fi;

        store="$BSPWM_SCRATCHPAD/$desktop";
        if [ ! -f "$store" ]; then exit 1; fi;

        while [ -s "$store" ]; do
          window=$(tail -n 1 "$store");
          sed -i '$ d' "$store";
          bspc node "$window" --flag hidden=off --focus && exit 0;
        done;

        exit 1;
      '';

  scratchpad-next = 
    pkgs.writeShellScriptBin "bspwm-scratchpad-next"
    ''
      desktop=$(bspc query --desktops --desktop);
      if [ -z "$desktop" ]; then exit 1; fi;

      store="$BSPWM_SCRATCHPAD/$desktop";
      if [ ! -f "$store" ]; then exit 1; fi;

      state="tiled";
      node=$(bspc query --nodes --node "focused.$state");

      if [ -z "$node" ]; then
        state="pseudo_tiled";
        node=$(bspc query --nodes --node "focused.$state");
      fi;

      if [ -z "$node" ]; then
        state="fullscreen";
        node=$(bspc query --nodes --node "focused.$state");
      fi;

      if [ -z "$node" ]; then
        state="floating";
        node=$(bspc query --nodes --node "focused.$state");
      fi;

      while [ -s "$store" ]; do
        window=$(tail -n 1 "$store");

        if [ -z "$(bspc query --nodes --node "$window")" ]; then
          sed -i '$ d' "$store";
          continue;
        fi;

        if [ "$node" ]; then
          sed -i "1s;^;$node\n;" "$store";
          bspc node "$node" --swap "$window" --flag hidden=on;
        fi;

        sed -i '$ d' "$store";
        bspc node "$window" --flag hidden=off --focus --state "$state" && exit 0;
      done;

      exit 1;
    '';

  scratchpad-prev = 
    pkgs.writeShellScriptBin "bspwm-scratchpad-prev"
      ''
        desktop=$(bspc query --desktops --desktop);
        if [ -z "$desktop" ]; then exit 1; fi;

        store="$BSPWM_SCRATCHPAD/$desktop";
        if [ ! -f "$store" ]; then exit 1; fi;

        state="tiled";
        node=$(bspc query --nodes --node "focused.$state");

        if [ -z "$node" ]; then
          state="pseudo_tiled";
          node=$(bspc query --nodes --node "focused.$state");
        fi;

        if [ -z "$node" ]; then
          state="fullscreen";
          node=$(bspc query --nodes --node "focused.$state");
        fi;

        if [ -z "$node" ]; then
          state="floating";
          node=$(bspc query --nodes --node "focused.$state");
        fi;

        while [ -s "$store" ]; do
          window=$(head -n 1 "$store");

          if [ -z "$(bspc query --nodes --node "$window")" ]; then
            sed -i '1d' "$store";
            continue;
          fi;

          if [ "$node" ]; then
            echo "$node" >> "$store";
            bspc node "$node" --swap "$window" --flag hidden=on;
          fi;

          sed -i '1d' "$store";
          bspc node "$window" --flag hidden=off --focus --state "$state" && exit 0;
        done;

        exit 1;
      '';
in
{
  options.module.bspwm = {
    enable = lib.mkEnableOption "Enable bspwm module";
    amount = lib.mkOption {
      type = types.int;
      default = 100;
    };
  };

  config = lib.mkIf cfg.enable {
    home.sessionVariables.DESKTOP_SESSION = "bspwm";
    home.sessionVariables.BSPWM_SCRATCHPAD = "$(mktemp -d)";

    xsession.initExtra = "systemctl --user import-environment DESKTOP_SESSION";

    xsession.windowManager.bspwm = {
      enable = true;
      package = pkgs.bspwm.overrideAttrs (old: {
        src = pkgs.fetchFromGitHub {
          owner = "baskerville";
          repo = "bspwm";
          rev = "af3bd8b4351f4478fe0fe3cfd6c09e44cb108b4b";
          hash = "sha256-O4Qwdjb3p6jw8Qtcd4zGZ57cB3oCCbPZcjUQtWbyC7Y=";
        };
      });
      /*
      extraConfig =
        ''
          # HACK: from https://www.reddit.com/r/bspwm/comments/fkgc94/monocle_true_transparency_hiding_not_focused_node/
          # TODO: remake this using picom-trans and add support for fullscreen windows

          #### Transparency hack: works by hiding previous focused nodes
          manage_node_transparency () {
            # Store last ids
            PREV_NODE_ID=
            PREV_DESK_ID=
            # On envery focus change
            bspc subscribe node_focus | while read -r _ _ desktop_id node_id; do
              # Set my custom _COMPTON_MONOCLE xprop to 1, meaning active
              xprop -id "$node_id" -f _COMPTON_MONOCLE 32c -set _COMPTON_MONOCLE 1;
              # Check if previous desktop is the same of now (no need to hide when changing desktops)
              if [ "$PREV_DESK_ID" == "$desktop_id" ] && [ "$PREV_NODE_ID" != "$node_id" ]; then
                # Get the layout of the previous node to hide
                PREV_LAYOUT=$(bspc query -T -d "$desktop_id" | jq -r .layout);
                # Hide the previous node only if it was on a monocle desktop
                [ "$PREV_LAYOUT" == "monocle" ] && xprop -id "$PREV_NODE_ID" -f _COMPTON_MONOCLE 32c -set _COMPTON_MONOCLE 0;
              fi
              # Update previous ids
              PREV_NODE_ID=$node_id
              PREV_DESK_ID=$desktop_id
            done

          }

          #### Manage Desktop Layout Change
          manage_desktop_layout () {
            # Subscribe to layout change event
            bspc subscribe desktop_layout | while read -r _ _ desktop_id layout; do
              if [ "$layout" == "tiled" ]; then
                # If tiled set _COMPTON_MONOCLE to all window nodes
                bspc query -N -n .window -d "$desktop_id" | xargs -I % xprop -id % -f _COMPTON_MONOCLE 32c -set _COMPTON_MONOCLE 1;
              else
                # Else, set 0 to all non focused window nodes
                bspc query -N -n .window.!focused -d "$desktop_id" | xargs -I % xprop -id % -f _COMPTON_MONOCLE 32c -set _COMPTON_MONOCLE 0;
              fi
            done
          }

          manage_node_transparency &
          manage_desktop_layout &
        '';
      */
      rules = {
        "Yad".floating = true;
        "copyq" = {
          state = "floating";
          rectangle = "1280x960+0+0";
          center = true;
        };
        "qalculate-qt" = {
          state = "floating";
          rectangle = "768x640+0+0";
          center = true;
        };
        "pavucontrol-qt" = {
          state = "floating";
          rectangle = "1115x839+0+0";
          center = true;
        };
        "bluedevil-wizard" = {
          state = "floating";
          rectangle = "750x678+0+0";
          center = true;
        };
        "pcmanfm-qt" = {
          state = "floating";
          center = true;
        };
        "TelegramDesktop" = {
          state = "floating";
          center = true;
        };
        "WebCord" = {
          state = "floating";
          center = true;
        };
        "Alacritty" = {
          state = "floating";
          center = true;
        };
        "org.wezfurlong.wezterm" = {
          state = "floating";
          center = true;
        };
        "Whatsapp-for-linux" = {
          state = "floating";
          center = true;
        };
      };
      settings = {
        border_width = 2;
        window_gap = 32;

        # Borders and gaps
        borderless_monocle = true;
        gapless_monocle = true;
        single_monocle = true;

        # Border colors
        normal_border_color = colors.blackBright;
        focused_border_color = colors.foreground;
        borderless_singleton = true;

        # fixed desktops
        remove_disabled_monitors = lib.mkDefault false;
        remove_unplugged_monitors = lib.mkDefault false;

        # Focusing behavior
        focus_follows_pointer = false;

        # Misc
        split_ratio = 0.50;
        initial_polarity = "second_child"; # first_child
        automatic_scheme = "longest_side"; # alternate
        pointer_motion_interval = "16";
      };
    };

    module.sxhkd.keybindings = {
      ######## #### ## #
      # MISC
      ######## #### ## #

      # panic restart bspwm
      "super + shift + Escape" = "bspc wm --restart";

      # panic quit bspwm
      "ctrl + alt + Escape" = "bspc quit";

      ######## #### ## #
      # WINDOW CONTROLS
      ######## #### ## #

      # ## #
      # primary actions (close/maximize)
      # ## #

      # [c]lose app
      "super + c" = "bspc node --close";
      # [c]lose app (kill)
      "super + shift + c" = "bspc node -k";

      # toggle ma[x]imize state (fullscreen)
      "super + x" = "bspc node --state ~fullscreen";

      # ## #
      # secondary actions (layout/pseudo float/fix rotate brother)
      # ## #

      # cycle [t]iled/monocle layout
      "super + t" = "bspc desktop --layout next";
      # toggle pseudo [t]iled state
      "super + shift + t" = ''
        bspc node focused.!pseudo_tiled --state pseudo_tiled
        ||
        bspc node focused.pseudo_tiled --state tiled
      '';

      # toggle [f]loating state
      "super + f" = "bspc node --state ~floating";
      # toggle [f]ixed state (sticky)
      "super + shift + f" = "bspc node --flag sticky";

      # [r]otate clockwise
      "super + r" = "bspc node '@parent' --rotate 90";
      # [r]otate counter-clockwise
      "super + shift + r" = "bspc node '@parent' --rotate -90";

      # focus brother
      "super + b" = "bspc node --focus @brother";
      # swap brother
      "super + shift + b" = "bspc node --swap @brother";

      # ## #
      # tertiary actions (show desktop/equalize/balance)
      # ## #

      # TOO LAZY TO FIX THIS NOW
      /*
      # show/hide [d]ekstop
      "super + d" =
        ''
          while bspc node 'any.local.!hidden.window' --flag hidden=on; do false; done
            && while bspc node 'any.local.hidden.window' --flag hidden=off; do :; done
        '';
      # hide/show [d]ekstop
      "super + shift + d" =
        ''
          while bspc node 'any.local.hidden.window' --flag hidden=off; do false; done
            && while bspc node 'any.local.!hidden.window' --flag hidden=on; do :; done
        '';
      */

      # focus [p]arent
      "super + p" = "bspc node --focus @parent";
      # focus first child
      "super + comma" = "bspc node --focus @first";
      # focus second child
      "super + period" = "bspc node --focus @second";

      # toggle gaps
      "super + g" =
        ''
          gaps=$(bspc config 'window_gap');
          if [ "$gaps" -eq "0" ]; then
            bspc config 'window_gap' 32;
          else
            bspc config 'window_gap' 0;
          fi
        '';

      ######## #### ## #
      # RECEPTACLES
      ######## #### ## #

      "super + i" =
        ''
          existing=$(bspc query --nodes --node any.local.leaf.!window);
          if [ "${dollar}{existing}" ]; then
            while bspc node 'any.local.leaf.!window' -k; do :; done
          else
            bspc node --insert-receptacle;
          fi
        '';

      "super + shift + i" =
        ''
          existing=$(bspc query --nodes --node 'prev.local.leaf.!window');
          if [ "$existing" ]; then bspc node --to-node "$existing"; fi;
        '';

      ######## #### ## #
      # FOCUS AND MOVEMENT
      ######## #### ## #

      # focus with vim keys
      "super + h" = focus "west";
      "super + j" = focus "south";
      "super + k" = focus "north";
      "super + l" = focus "east";

      # focus with arrow keys
      "super + Left" = focus "west";
      "super + Down" = focus "south";
      "super + Up" = focus "north";
      "super + Right" = focus "east";

      # move with vim keys
      "super + shift + h" = move "west";
      "super + shift + j" = move "south";
      "super + shift + k" = move "north";
      "super + shift + l" = move "east";

      # move with arrow keys
      "super + shift + Left" = move "west";
      "super + shift + Down" = move "south";
      "super + shift + Up" = move "north";
      "super + shift + Right" = move "east";

      # focus floating windows
      "super + space" =
        ''
          bspc node 'focused.!floating' -f 'last.local.!hidden.floating.window'
          ||
          bspc node 'focused.floating' -f 'last.local.!hidden.!floating.window'
        '';

      # focus previous window
      "alt + Tab" = 
        ''
          ${lock} ${switch}/bin/bspwm-switch "older" "newer"
        '';
      # focus next window
      "alt + shift + Tab" =
        ''
          ${lock} ${switch}/bin/bspwm-switch "newer" "older"
        '';

      ######## #### ## #
      # WORKSPACES
      ######## #### ## #

      # focus workspace
      "super + 1" = "bspc desktop --focus '^1'";
      "super + 2" = "bspc desktop --focus '^2'";
      "super + 3" = "bspc desktop --focus '^3'";
      "super + 4" = "bspc desktop --focus '^4'";
      "super + 5" = "bspc desktop --focus '^5'";
      "super + 6" = "bspc desktop --focus '^6'";
      "super + 7" = "bspc desktop --focus '^7'";
      "super + 8" = "bspc desktop --focus '^8'";
      "super + 9" = "bspc desktop --focus '^9'";
      "super + 0" = "bspc desktop --focus '^10'";
      "super + apostrophe" = "bspc desktop --focus '^11'";

      # move window to workspace
      "super + shift + 1" = "bspc node --to-desktop '^1'";
      "super + shift + 2" = "bspc node --to-desktop '^2'";
      "super + shift + 3" = "bspc node --to-desktop '^3'";
      "super + shift + 4" = "bspc node --to-desktop '^4'";
      "super + shift + 5" = "bspc node --to-desktop '^5'";
      "super + shift + 6" = "bspc node --to-desktop '^6'";
      "super + shift + 7" = "bspc node --to-desktop '^7'";
      "super + shift + 8" = "bspc node --to-desktop '^8'";
      "super + shift + 9" = "bspc node --to-desktop '^9'";
      "super + shift + 0" = "bspc node --to-desktop '^10'";
      "super + shift + apostrophe" = "bspc node --to-desktop '^11'";

      # focus next/prev workspace
      "super + minus" = "bspc desktop --focus prev";
      "super + equal" = "bspc desktop --focus next";

      # move workspace
      "super + shift + minus" = "bspc desktop --swap prev --follow";
      "super + shift + equal" = "bspc desktop --swap next --follow";

      ######## #### ## #
      # RESIZE
      ######## #### ## #

      "super + s :" =
        let
          grow-left = "bspc node --resize left -${amount} 0";
          grow-down = "bspc node --resize bottom 0 ${amount}";
          grow-up = "bspc node --resize top 0 -${amount}";
          grow-right = "bspc node --resize right ${amount} 0";

          shrink-left = "bspc node --resize left ${amount} 0";
          shrink-down = "bspc node --resize bottom 0 -${amount}";
          shrink-up = "bspc node --resize top 0 ${amount}";
          shrink-right = "bspc node --resize right -${amount} 0";
        in
        {
          # enlarge
          "Left" = grow-left;
          "h" = grow-left;
          "Down" = grow-down;
          "j" = grow-down;
          "Up" = grow-up;
          "k" = grow-up;
          "Right" = grow-right;
          "l" = grow-right;

          # shrink
          "shift + Left" = shrink-left;
          "shift + h" = shrink-left;
          "shift + Down" = shrink-down;
          "shift + j" = shrink-down;
          "shift + Up" = shrink-up;
          "shift + k" = shrink-up;
          "shift + Right" = shrink-right;
          "shift + l" = shrink-right;

          # [e]qualize spacing
          "e" =
            ''
              bspc node @parent --equalize;
              ${escape}
            '';
          # [e]qualize spacing for all
          "shift + e" =
            ''
              bspc node @/ --equalize;
              ${escape}
            '';

          # [b]alance spacing
          "b" =
            ''
              bspc node @parent --balance;
              ${escape}
            '';
          # [b]alance spacing for all
          "shift + b" =
            ''
              bspc node @/ --balance;
              ${escape}
            '';

          # cancel
          "Return" = escape;
          "space" = escape;
          "s" = escape;
        };

      ######## #### ## #
      # SCRATCHPAD
      ######## #### ## #

      # [m]inimize window (hide)
      "super + m" =
        ''
          ${lock} ${minimize}/bin/bspwm-minimize
        '';

      # un[m]inimize window (unhide)
      "super + shift + m" =
        ''
          ${lock} ${restore}/bin/bspwm-restore
        '';

      # swap current window with previous scratchpad window
      "super + Tab" =
        ''
          ${lock} ${scratchpad-next}/bin/bspwm-scratchpad-next
        '';

      # swap current window with next scratchpad window
      "super + shift + Tab" =
        ''
          ${lock} ${scratchpad-prev}/bin/bspwm-scratchpad-prev
        '';
    };
  };
}
