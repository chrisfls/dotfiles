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

  focus = direction:
    ''
      bspc node 'focused.!floating' --focus '${direction}.!floating'
      ||
      bspc node 'focused.floating' --focus '${direction}.floating'
    '';

  swapOrMove = direction:
    let
      n = toString cfg.amount;
      prefix = "bspc node 'focused.!floating' --swap ${direction}";
    in
    if direction == "west" then
      "${prefix} || bspc node 'focused.floating' --move -${n} 0"
    else if direction == "south" then
      "${prefix} || bspc node 'focused.floating' --move 0 ${n}"

    else if direction == "north" then
      "${prefix} || bspc node 'focused.floating' --move 0 -${n}"

    else if direction == "east" then
      "${prefix} || bspc node 'focused.floating' --move ${n} 0"

    else
      builtins.throw "Invalid move direction";

  # (bspc node '${direction}' -p 'south' && bspc node -n 'last.!automatic.local')

  resize = vertical: grow:
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

      # rotate parent container
      "super + r" = "bspc node '@parent' --rotate 90";
      "super + shift + r" = "bspc node '@parent' --rotate -90";

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

      # TODO: hide/unhide window

      # TODO: swap this window with hidden
      "super + n" = "bspc node 'prev.hidden' --flag hidden=off --swap focused";
      "super + shift + n" = "bspc node --flag hidden=on";


      /*

        default = {

        # WINDOW CONTROLS
        ######## #### ## #

        "super + shift + Tab" =
      "node -f next.local.!hidden.window";

        #"super + q" = "";
        #"super + s" = "";
        #"super + g" = "";
        #"super + f" = "";
        #"super + space" = "";
        };

      */


      "super + Tab" =
        ''
          bspc node $(bspc query --nodes --node 'next.hidden.window') --flag hidden=off
        '';
      #"super + shift + tab" = "";

      #"super + Tab" = "bspc node --focus prev.!hidden.window.local";
      #"super + shift + Tab" = "bspc node --focus next.!hidden.window.local";

      "alt + Tab" =
        ''
          bspc node 'focused.!floating' --focus 'prev.!floating.window.local' --flag hidden=off
          ||
          bspc node 'focused.floating' --focus 'prev.floating.window.local' --flag hidden=off
        '';

      "alt + shift + Tab" =
        ''
          bspc node 'focused.!floating' --focus 'next.!floating.window.local' --flag hidden=off
          ||
          bspc node 'focused.floating' --focus 'next.floating.window.local' --flag hidden=off
        '';

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

      # (RE) SIZE
      ######## #### ## #

      "super + s :" =
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

# SAMPLE BSPWM configs:
/*
  #
  # wm independent sxhkd
  #

  # terminal emulator
  super + Return
      urxvt

  # program launcher
  super + @space
      dmenu_run


  #
  # bspwm sxhkd
  #


  # alternate between the tiled and monocle layout
  super + m
      bspc desktop -l next

  # send the newest marked node to the newest preselected node
  super + y
      bspc node newest.marked.local -n newest.!automatic.local

  # swap the current node and the biggest window
  super + g
      bspc node -s biggest.window

  #
  # state/flags
  #

  # set the window state
  super + {t,shift + t,s,f}
      bspc node --state {tiled,pseudo_tiled,floating,fullscreen}

  # set the node flags
  super + ctrl + {m,x,y,z}
      bspc node --flag {marked,locked,sticky,private}

  #
  # focus/swap
  #

  # focus the node in the given direction
  super + {_,shift + }{h,j,k,l}
      bspc node -{f,s} {west,south,north,east}

  # focus the node for the given path jump
  super + {p,b,comma,period}
      bspc node -f @{parent,brother,first,second}

  # focus the next/previous window in the current desktop
  super + {_,shift + }c
      bspc node -f {next,prev}.local.!hidden.window

  # focus the next/previous desktop in the current monitor
  super + bracket{left,right}
      bspc desktop -f {prev,next}.local

  # focus the last node/desktop
  super + {grave,Tab}
      bspc {node,desktop} -f last

  # focus the older or newer node in the focus history
  super + {o,i}
      bspc wm -h off; \
      bspc node {older,newer} -f; \
      bspc wm -h on

  # focus or send to the given desktop
  super + {_,shift + }{1-9,0}
      bspc {desktop -f,node -d} '^{1-9,10}'

  #
  # preselect
  #

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

  #
  # move/resize
  #

  # expand a window by moving one of its side outward
  super + alt + {h,j,k,l}
      bspc node -z {left -20 0,bottom 0 20,top 0 -20,right 20 0}

  # contract a window by moving one of its side inward
  super + alt + shift + {h,j,k,l}
      bspc node -z {right -20 0,top 0 20,bottom 0 -20,left 20 0}

  # move a floating window
  super + {Left,Down,Up,Right}
      bspc node -v {-20 0,0 20,0 -20,20 0}

  */
