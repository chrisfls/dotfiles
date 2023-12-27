{ config, lib, pkgs, ... }:
let
  inherit (builtins)
    attrNames
    attrValues
    concatStringsSep
    filter
    map
    throw
    ceil
    toString;

  inherit (lib.attrsets)
    mapAttrs';

  inherit (lib.strings)
    splitString
    removePrefix
    removeSuffix;

  inherit (lib.trivial)
    pipe;

  inherit (lib)
    types
    isAttrs;

  cfg = config.module.sxhkd;

  xdotool = "${pkgs.xdotool}/bin/xdotool";

  dollar = "$";

  amount = toString cfg.amount;

  focus = direction:
    ''
      bspc node 'focused.!floating' --focus '${direction}.!floating.window'
      ||
      bspc node 'focused.floating' --focus '${direction}.floating.window'
    '';

  swapOrMove = direction:
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
      builtins.throw "Invalid move direction";

  # (bspc node '${direction}' -p 'south' && bspc node -n 'last.!automatic.local')

  i3-resize = vertical: grow:
    let
      fmt = n: toString (ceil n);
      n = if grow then cfg.amount / -2 else cfg.amount / 2;
      fst = if vertical then "top 0 ${fmt n}" else "left ${fmt n} 0";
      snd = if vertical then "bottom 0 ${fmt (n * -1)}" else "right ${fmt (n * -1)} 0";
    in
    ''
      bspc node --resize ${fst} --resize ${snd}
    '';
in
{
  options.module.sxhkd = {
    enable = lib.mkEnableOption "Enable sxhkd module";
    amount = lib.mkOption {
      type = types.int;
      default = 100;
    };
    keybindings = lib.mkOption {
      type = types.lazyAttrsOf (types.oneOf [ types.str (types.lazyAttrsOf types.str) ]);
      default = { };
    };
  };

  config = lib.mkIf cfg.enable {
    # TODO: move to bspwm
    module.sxhkd.keybindings = {
      # MISC
      ######## #### ## #

      # reload sxhkd configs
      "super + Escape" = "pkill -USR1 -x sxhkd";

      # panic restart bspwm
      "super + shift + Escape" = "bspc wm --restart";

      # panic quit bspwm
      "control + alt + Escape" = "bspc quit";


      # WINDOW CONTROLS
      ######## #### ## #

      # close app
      "super + c" = "bspc node --close";
      # kill app
      "super + shift + c" = "bspc node -k";

      # shift pair direction
      "super + s" = "bspc node '@parent.vertical' -y horizontal || bspc node '@parent' -y vertical";
      # flip pair direction
      "super + shift + s" = "bspc node '@parent' --rotate 180";

      # toggle floating state
      "super + f" = "bspc node --state ~floating";

      # toggle maximize state (fullscreen)
      "super + x" = "bspc node --state ~fullscreen";

      # cycle tiled/monocle layout
      "super + t" = "bspc desktop --layout next";

      # toggle pseudo tiled state
      "super + p" = "bspc node --state ~pseudo_tiled";
      # toggle pinned flag (sticky)
      "super + shift + p" = "bspc node --flag sticky";

      # minimize window (hide)
      "super + m" = "bspc node --flag hidden=on";
      # unminimize window (unhide)
      "super + shift + m" = "bspc node 'any.local.hidden.window' --flag hidden=off;";

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
      "super + shift + h" = swapOrMove "west";
      "super + shift + j" = swapOrMove "south";
      "super + shift + k" = swapOrMove "north";
      "super + shift + l" = swapOrMove "east";

      # move with arrow keys
      "super + shift + Left" = swapOrMove "west";
      "super + shift + Down" = swapOrMove "south";
      "super + shift + Up" = swapOrMove "north";
      "super + shift + Right" = swapOrMove "east";

      # toggle floating focus
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

      # MANUAL SWALLOW
      ######## #### ## #

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

      # RESIZE
      ######## #### ## #

      "super + r :" =
        let
          escape = "${xdotool} key Escape";
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

          "Left" = grow-left; # i3-resize false false;
          "h" = grow-left; # i3-resize false false;

          "Down" = grow-down; # i3-resize true true;
          "j" = grow-down; # i3-resize true true;

          "Up" = grow-up; # i3-resize true false;
          "k" = grow-up; # i3-resize true false;

          "Right" = grow-right; # i3-resize false true;
          "l" = grow-right; # i3-resize false true;

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

      # TODO: perhaps join both modes down

      # RECEPTACLE
      ######## #### ## #

      "super + shift + r ;" =
        let
          escape = "${xdotool} key Escape";
          move = dir1: dir2:
            ''
              focused=$(bspc query --nodes --node);
              bspc node $(bspc query --nodes @parent --node '${dir1}.local.!hidden.!floating.window') --presel-dir ${dir2} --insert-receptacle
              &&
              bspc node $focused --to-node $(bspc query --nodes --node 'prev.leaf.!window') --focus $focused;
            '';
          move-left = move "west" "east";
          move-down = move "south" "north";
          move-up = move "north" "south";
          move-right = move "east" "west";
        in
        {
          "Left" = move-left;
          "h" = move-left;

          "Down" = move-down;
          "j" = move-down;

          "Up" = move-up;
          "k" = move-up;

          "Right" = move-right;
          "l" = move-right;

          "Return" = escape;
          "space" = escape;
          "r" = escape;
        };

      # remove receptacles that might have been created by mistake
      "super + alt + r" = "while bspc node 'any.leaf.!window' -k; do :; done";

      # PRESELECTION
      ######## #### ## #

      "super + g ;" = # grab
        let
          escape = "${xdotool} key Escape";
        in
        {
          "Left" = "bspc node --presel-dir ~west";
          "h" = "bspc node --presel-dir ~west";

          "Down" = "bspc node --presel-dir ~south";
          "j" = "bspc node --presel-dir ~south";

          "Up" = "bspc node --presel-dir ~north";
          "k" = "bspc node --presel-dir ~north";

          "Right" = "bspc node --presel-dir ~east";
          "l" = "bspc node --presel-dir ~east";

          "Return" = escape;
          "space" = escape;
          "s" = escape;
        };

      "super + shift + g" = # ungrab
        "bspc node --presel-dir cancel";

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
    };

    xsession.initExtra =
      ''
        export SXHKD_FIFO="$XDG_RUNTIME_DIR/sxhkd.fifo"
        [ -e "$SXHKD_FIFO" ] && rm "$SXHKD_FIFO"
        mkfifo "$SXHKD_FIFO"
        systemctl --user import-environment SXHKD_FIFO
      '';

    home.sessionVariables.SXHKD_SHELL = "${pkgs.dash}/bin/dash";

    services.sxhkd = {
      enable = true;
      package = pkgs.sxhkd.overrideAttrs (old: {
        src = pkgs.fetchFromGitHub {
          owner = "baskerville";
          repo = "sxhkd";
          rev = "b0923b6b3d5d85b1ff80a5bb286934b8721a7c08";
          hash = "sha256-cSJZveU4quKT2MG1H8/cz9JEaj6Z4t0AEViPah4QoPs=";
        };
        patches = old.patches ++ [
          (pkgs.fetchpatch {
            name = "sxhkd-line-length.patch";
            url = "https://github.com/kress95/sxhkd/commit/d8ee372042618d6b603fce19d8e3035095f1fe28.patch";
            hash = "sha256-DzFtui42M9fUB7wXkgJzGmZtC6pgQXaZhKoOJ7cTB0A=";
          })
        ];
      });
      extraOptions = [ "-s $SXHKD_FIFO" ];
      keybindings =
        let
          trim = str:
            let
              str' = (removePrefix " " (removeSuffix " " str));
            in
            if str == str' then
              str
            else
              trim str';

          format = str:
            pipe str [
              (splitString "\n")
              (map trim)
              (filter (str: str != ""))
              (concatStringsSep " ")
            ];
        in
        mapAttrs'
          (name: value:
            if isAttrs value then
              let
                names = attrNames value;
                values = attrValues value;
              in
              {
                name = "${name} {${concatStringsSep ", " names}}";
                value = "{ ${concatStringsSep " , " (map format values)} }";
              }

            else
              { inherit name; value = format value; })
          cfg.keybindings;
    };
  };
}
