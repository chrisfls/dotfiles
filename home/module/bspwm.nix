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
      "super + r" = "bspc node '@parent.vertical' -y horizontal || bspc node '@parent' -y vertical";
      # [r]everse pair position
      "super + shift + r" = "bspc node '@parent' --rotate 180";

      # toggle [p]seudo tiled state
      "super + p" = "bspc node --state ~pseudo_tiled";
      # toggle [p]inned flag (sticky)
      "super + shift + p" = "bspc node --flag sticky";

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
          bspc node 'focused.!floating' --focus 'prev.local.!hidden.!floating.window' --flag hidden=off
          ||
          bspc node 'focused.floating' --focus 'prev.local.!hidden.floating.window' --flag hidden=off
        '';
      # focus next window
      "alt + shift + Tab" =
        ''
          bspc node 'focused.!floating' --focus 'next.local.!hidden.!floating.window' --flag hidden=off
          ||
          bspc node 'focused.floating' --focus 'next.local.!hidden.floating.window' --flag hidden=off
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


          # TODO: make work with all windows
          # i3-resize = vertical: grow:
          #   let
          #     fmt = n: toString (builtins.ceil n);
          #     n = if grow then cfg.amount / -2 else cfg.amount / 2;
          #     fst = if vertical then "top 0 ${fmt n}" else "left ${fmt n} 0";
          #     snd = if vertical then "bottom 0 ${fmt (n * -1)}" else "right ${fmt (n * -1)} 0";
          #   in
          #   ''
          #     bspc node --resize ${fst} --resize ${snd}
          #   '';
          # resize-left = i3-resize false false;
          # resize-down = i3-resize true true;
          # resize-up = i3-resize true false;
          # resize-right = i3-resize false true;
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

          # enlarge and shrink
          # "super + Left" = resize-left;
          # "super + h" = resize-left;
          # "super + Down" = resize-down;
          # "super + j" = resize-down;
          # "super + Up" = resize-up;
          # "super + k" = resize-up;
          # "super + Right" = resize-right;
          # "super + l" = resize-right;

          # cancel
          "Return" = escape;
          "space" = escape;
          "s" = escape;
        };

      ######## #### ## #
      # INSERT
      ######## #### ## #

      "super + i ;" =
        let
          insert = dir:
            let
              dir' =
                if dir == "west" then
                  "east"
                else if dir == "south" then
                  "north"
                else if dir == "north" then
                  "south"
                else if dir == "east" then
                  "west"
                else
                  throw "Invalid move direction";
            in
            ''
              focused=$(bspc query --nodes --node);
              bspc node $(bspc query --nodes @parent --node '${dir}.local.!hidden.!floating.window') --presel-dir ${dir'} --insert-receptacle
              &&
              bspc node $focused --to-node $(bspc query --nodes --node 'prev.leaf.!window') --focus $focused;
            '';
        in
        {
          # insert at
          "Left" = insert "west";
          "h" = insert "west";
          "Down" = insert "south";
          "j" = insert "south";
          "Up" = insert "north";
          "k" = insert "north";
          "Right" = insert "east";
          "l" = insert "east";

          # cancel
          "Return" = "";
          "space" = "";
          "i" = "";
        };

      # remove all receptacles
      "super + shift + i" = "while bspc node 'any.leaf.!window' -k; do :; done";

      ######## #### ## #
      # HIDDEN SWALLOW
      ######## #### ## #

      # TODO: fix focus flicker

      # swap current window with previous hidden window
      "super + Tab" =
        ''
          hidden=$(bspc query --nodes --node 'next.local.hidden.window');
          if [ "${dollar}{hidden}" ]; then
            focused=$(bspc query --nodes --node 'focused.local.window');
            unfocused=$(bspc query --nodes --node 'prev.local.!hidden.window');
            if [ "${dollar}{unfocused}" ]; then
              bspc node --presel-dir north --insert-receptacle --flag hidden=on
              &&
              bspc node $hidden --to-node $(bspc query --nodes --node 'prev.leaf.!window') --flag hidden=off --focus $hidden;
            elif [ "${dollar}{focused}" ]; then
              bspc node $focused --flag hidden=on
              &&
              bspc node $hidden --flag hidden=off --focus $hidden;
            else
              bspc node $hidden --flag hidden=off --focus $hidden;
            fi;
          else
            bspc node 'any.local.hidden.window' --flag hidden=off;
          fi;
        '';

      # swap current window with next hidden window
      "super + shift + Tab" =
        ''
          hidden=$(bspc query --nodes --node 'prev.local.hidden.window');
          if [ "${dollar}{hidden}" ]; then
            focused=$(bspc query --nodes --node 'focused.local.window');
            unfocused=$(bspc query --nodes --node 'prev.local.!hidden.window');
            if [ "${dollar}{unfocused}" ]; then
              bspc node --presel-dir north --insert-receptacle --flag hidden=on
              &&
              bspc node $hidden --to-node $(bspc query --nodes --node 'prev.leaf.!window') --flag hidden=off --focus $hidden;
            elif [ "${dollar}{focused}" ]; then
              bspc node $focused --flag hidden=on;
              bspc node $hidden --flag hidden=off --focus $hidden;
            else
              bspc node $hidden --flag hidden=off --focus $hidden;
            fi;
          else
            bspc node 'any.local.hidden.window' --flag hidden=off;
          fi;
        '';
    };
  };
}
