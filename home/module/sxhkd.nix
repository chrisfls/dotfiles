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

  # bspc node 'south' -p 'east' && bspc node -n 'last.!automatic.local'

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
      prefix = "bspc node 'focused.!floating' --swap ${direction}";
    in
    if direction == "west" then
      "${prefix} || bspc node 'focused.floating' --move -${amount} 0"
    else if direction == "south" then
      "${prefix} || bspc node 'focused.floating' --move 0 ${amount}"

    else if direction == "north" then
      "${prefix} || bspc node 'focused.floating' --move 0 -${amount}"

    else if direction == "east" then
      "${prefix} || bspc node 'focused.floating' --move ${amount} 0"

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
      default = 50;
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
      "super + shift + q" = "bspc quit";

      # WINDOW CONTROLS
      ######## #### ## #

      # close app
      "super + c" = "bspc node --close";
      # kill app
      "super + shift + c" = "bspc node -k";

      # shift direction
      "super + s" = "bspc node '@parent.vertical' -y horizontal || bspc node '@parent' -y vertical";

      # toggle floating state
      "super + f" = "bspc node --state ~floating";

      # toggle monocle layout
      "super + m" = "bspc desktop --layout next";
      # toggle fullscreen state
      "super + shift + m" = "bspc node --state ~fullscreen";

      # toggle pseudo tiled state
      "super + p" = "bspc node --state ~pseudo_tiled";
      # toggle sticky flag
      "super + shift + p" = "bspc node --flag sticky";

      # hide window
      "super + n" = "bspc node --flag hidden=on";
      # unhide window
      "super + shift + n" = "bspc node 'prev.hidden' --flag hidden=off";

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
          bspc node 'focused.!floating' -f 'last.!hidden.floating.local'
          ||
          bspc node 'focused.floating' -f 'last.!hidden.!floating.local'
        '';

      # focus previous window
      "alt + Tab" =
        ''
          bspc node 'focused.!floating' --focus 'prev.local.window.!floating' --flag hidden=off
          ||
          bspc node 'focused.floating' --focus 'prev.local.window.floating' --flag hidden=off
        '';

      # focus next window
      "alt + shift + Tab" =
        ''
          bspc node 'focused.!floating' --focus 'next.local.window.!floating' --flag hidden=off
          ||
          bspc node 'focused.floating' --focus 'next.local.window.floating' --flag hidden=off
        '';

      # TAB Simulation
      ######## #### ## #

      # swap current window with previous hidden window
      "super + Tab" =
        ''
          wid=$(bspc query --nodes --node 'prev.local.window.hidden');
          if [ "${dollar}{wid}" ]; then
            bspc node --presel-dir north -i --flag hidden=on
            &&
            bspc node $wid --flag hidden=off --to-node $(bspc query --nodes --node 'prev.leaf.!window') --focus $wid
          ; fi
        '';

      # swap current window with next hidden window
      "super + shift + Tab" =
        ''
          wid=$(bspc query --nodes --node 'next.local.window.hidden');
          if [ "${dollar}{wid}" ]; then
            bspc node --presel-dir north -i --flag hidden=on
            &&
            bspc node $wid --flag hidden=off --to-node $(bspc query --nodes --node 'prev.leaf.!window') --focus $wid
          ; fi
        '';

      # RESIZE
      ######## #### ## #

      "super + r :" =
        # TODO directional expand/contract
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
          "Left" = grow-left; # i3-resize false false;
          "h" = grow-left; # i3-resize false false;

          "Down" = grow-down; # i3-resize true true;
          "j" = grow-down; # i3-resize true true;

          "Up" = grow-up; # i3-resize true false;
          "k" = grow-up; # i3-resize true false;

          "Right" = grow-right; # i3-resize false true;
          "l" = grow-right; # i3-resize false true;

          "shift + Left" = shrink-left; # i3-resize false false;
          "shift + h" = shrink-left; # i3-resize false false;

          "shift + Down" = shrink-down; # i3-resize true true;
          "shift + j" = shrink-down; # i3-resize true true;

          "shift + Up" = shrink-up; # i3-resize true false;
          "shift + k" = shrink-up; # i3-resize true false;

          "shift + Right" = shrink-right; # i3-resize false true;
          "shift + l" = shrink-right; # i3-resize false true;

          "Return" = escape;
          "space" = escape;
          "s" = escape;
        };


      # (RE) SIZE
      ######## #### ## #

      /*
      "super + r :" =
        let
          escape = "${xdotool} key Escape";
        in
        {
          "Left" = resize false false;
          "h" = resize false false;

          "Down" = resize true true;
          "j" = resize true true;

          "Up" = resize true false;
          "k" = resize true false;

          "Right" = resize false true;
          "l" = resize false true;

          "Return" = escape;
          "space" = escape;
          "s" = escape;
        };
      */

      # PRESELECTION
      ######## #### ## #

      # TODO

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

# bspc node (bspc query --nodes --node 'prev.hidden') --flag hidden=off --to-node (bspc query --nodes --node '.leaf.!window') --follow

/*
  #
  # preselect
  #

  bspc node --presel-dir north -i && bspc node --presel-dir north -i
  bspc node 

  bspc node 'prev.hidden' --flag hidden=off

  # preselect the direction
  super + ctrl + {h,j,k,l}
      bspc node -p {west,south,north,east}

  # preselect the ratio
  super + ctrl + {1-9}
      bspc node -o 0.{1-9}

  # cancel the preselection for the focused node
  super + ctrl + space
      bspc node -p cancel

  # cancel the preselection for the focused desktop
  super + ctrl + shift + space
      bspc query -N -d | xargs -I id -n 1 bspc node id -p cancel

  */

