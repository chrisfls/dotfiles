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

  switch = age:
    let
      age' =
        if age == "newer" then
          "older"
        else if age == "older" then
          "newer"
        else
          throw "Invalid age";
    in
    ''
      bspc wm -h off;

      window=$(bspc query --nodes --node 'focused.local.!hidden.!floating.window');

      if [ "$window" ]; then
        modifier="!floating";
      else
        modifier="floating";
      fi;

      bspc node --focus "older.local.!hidden.$modifier.window" || (
        if [ -z "$window" ]; then
          window=$(bspc query --nodes --node);
        fi;

        while true; do
          result=$(bspc query --nodes --node "$window#newer.local.!hidden.$modifier.window");

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

  # TODO: https://github.com/JopStro/bspswallow
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
      monitors.PAD = [ "pad" ];
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
      };
      settings = {
        border_width = 2;
        window_gap = 18;

        # Borders and gaps
        borderless_monocle = true;
        gapless_monocle = true;
        single_monocle = true;

        # Border colors
        normal_border_color = colors.background;
        focused_border_color = colors.foreground;
        borderless_singleton = true;

        # disble multihead behavior (TODO: move to user)
        remove_disabled_monitors = true;
        remove_unplugged_monitors = true;

        # Focusing behavior
        focus_follows_pointer = false;

        # Misc
        split_ratio = 0.50;
        initial_polarity = "first_child";
        automatic_scheme = "alternate";
      };
    };

    module.sxhkd.keybindings = {
      ######## #### ## #
      # MISC
      ######## #### ## #

      # panic restart bspwm
      "super + shift + Escape" = "bspc wm --restart";

      # panic quit bspwm
      "control + alt + Escape" = "bspc quit";

      ######## #### ## #
      # WINDOW CONTROLS
      ######## #### ## #

      # ## #
      # primary actions (close/min/max)
      # ## #

      # [c]lose app
      "super + c" = "bspc node --close";
      # [c]lose app (kill)
      "super + shift + c" = "bspc node -k";

      # [m]inimize window (hide)
      "super + m" = "bspc node --flag hidden=on";
      # un[m]inimize window (unhide)
      "super + shift + m" = "bspc node 'any.local.hidden.window' --flag hidden=off;";

      # toggle ma[x]imize state (fullscreen)
      "super + x" = "bspc node --state ~fullscreen";

      # ## #
      # secondary actions (layout/float/rotate/reverse/pseudo/pin)
      # ## #

      # cycle [t]iled/monocle layout
      "super + t" = "bspc desktop --layout next";

      # toggle [f]loating state
      "super + f" = "bspc node --state ~floating";

      # [r]otate pair direction
      "super + s" = "bspc node '@parent.vertical' -y horizontal || bspc node '@parent' -y vertical";
      # [r]everse pair position
      "super + shift + s" = "bspc node '@parent' --rotate 180";

      # toggle [p]seudo tiled state
      "super + p" = "bspc node --state ~pseudo_tiled";
      # toggle [p]inned flag (sticky)
      "super + shift + p" = "bspc node --flag sticky";

      # ## #
      # tertiary actions (show desktop/equalize/balance)
      # ## #

      # show/hide [d]ekstop
      "super + d" =
        ''
          while bspc node any.local.hidden.window -g hidden=off; do false; done
            && while bspc node 'any.local.!hidden.window' -g hidden=on; do :; done
        '';

      "super + w ;" = {
        # [e]qualize spacing
        "e" = "bspc node @parent --equalize";
        # [e]qualize spacing for all
        "shift + e" = "bspc node @/ --equalize";

        # [b]alance spacing
        "b" = "bspc node @parent --balance";
        # [b]alance spacing for all
        "shift + b" = "bspc node @/ --balance";

        # cancel
        "Return" = escape;
        "space" = escape;
        "w" = escape;
      };

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
        "bspc node --to-node $(bspc query --nodes --node 'prev.local.leaf.!window') --focus $focused";

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
      "alt + Tab" = switch "older";
      # focus next window
      "alt + shift + Tab" = switch "newer";

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

      # focus next/prev workspace
      "super + minus" = "bspc desktop --focus prev";
      "super + equal" = "bspc desktop --focus next";

      # move workspace
      "super + shift + minus" = "bspc desktop --swap prev --follow";
      "super + shift + equal" = "bspc desktop --swap next --follow";

      ######## #### ## #
      # RESIZE
      ######## #### ## #

      "super + r :" =
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

          # cancel
          "Return" = escape;
          "space" = escape;
          "r" = escape;
        };

      ######## #### ## #
      # SCRATCHPAD
      ######## #### ## #

      # push window to scratchpad
      "super + apostrophe" = "bspc node --flag sticky=off --to-desktop pad";
      # pop window from scratchpad
      "super + shift + apostrophe" =
        ''
          pad=$(bspc query --desktops --desktop 'pad');
          bspc node "@$pad:" --swap "@$pad:/#prev"
          &&
          bspc node "@$pad:" --to-desktop focused
          bspc node "prev#@$pad:" --to-desktop focused
        '';

      # swap current window with previous scratchpad window
      "super + Tab" =
        ''
          pad=$(bspc query --desktops --desktop 'pad');
          bspc node "@$pad:/#next" --swap focused
          &&
          bspc node "@$pad:/#prev" --flag sticky=off --swap "@$pad:/#next"
        '';
      # swap current window with next scratchpad window
      "super + shift + Tab" =
        ''
          pad=$(bspc query --desktops --desktop 'pad');
          bspc node "@$pad:/#prev" --swap focused
          &&
          bspc node "@$pad:/#prev" --flag sticky=off --swap "@$pad:/#next"
        '';
    };
  };
}
# bspc node "@$pad:/#prev" --to-desktop focused
