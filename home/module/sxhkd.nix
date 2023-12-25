{ config, lib, pkgs, ... }:
let
  inherit (builtins)
    attrNames
    attrValues
    concatStringsSep
    filter
    map
    throw
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


  bspc-resize-x = pkgs.writeShellScriptBin "bspc-resize-x"
    ''
      if bspc query -N -n "focused.floating" > /dev/null; then
        val=$(($1 / 2))
      else
        val=$1
      fi

      bspc node -z left $(($val * -1)) 0
      bspc node -z right $val 0
    '';

  bspc-resize-y = pkgs.writeShellScriptBin "bspc-resize-y"
    ''
      if bspc query -N -n "focused.floating" > /dev/null; then
        val=$(($1 / 2))
      else
        val=$1
      fi

      bspc node -z top $(($val * -1)) 0
      bspc node -z bottom $val 0
    '';


  bspc-toggle-focus = pkgs.writeShellScriptBin "bspc-toggle-focus"
    ''
      if bspc query -N -n 'focused.floating' > /dev/null; then
        bspc node -f 'last.!hidden.!floating'
      else
        bspc node -f 'last.!hidden.floating'
      fi
    '';

  /*

    default = {

    # WINDOW CONTROLS
    ######## #### ## #

    # toggle tiled / monocle / fullscreen
    "super + {_,shift +} m" =
      "bspc {desktop -l next,node -t fullscreen}";

    # tiled / pseudo tiled
    "super + p" =
      "bspc node -t ~pseudo_tiled";

    # floating / pinned
    "super + f" =
      "bspc node -t ~floating";
    "super + shift + f" = # TODO: it would be good if only floating windows could be stity
      "bspc node -g sticky";

    # toggle fullscreen / monocle
    "super + m" =
      "bspc node -t fullscreen";
    "super + shift + m" =
      "bspc desktop -l next";

    # toggle float focus
    "super + Tab" =
      "node -f prev.local.!hidden.window";

    "super + shift + Tab" =
      "node -f next.local.!hidden.window";

    "super + space" = "bspc-toggle-focus";

    #"super + q" = "";
    #"super + r" = "";
    #"super + s" = "";
    #"super + g" = "";
    #"super + f" = "";
    #"super + {}" = "bspc node -t {tiled,pseudo_tiled,floating,fullscreen}";
    #"super + space" = "";

    # WINDOWING
    ######## #### ## #

    # focus windows
    "super + {h,j,k,l}" = "bspc-focus {west,south,north,east}";
    "super + {Left,Down,Up,Right}" = "bspc-focus {west,south,north,east}";

    # focus floating window


    # move windows
    "super + shift + {h,Left}" = "bspc-move-x -${amount}";
    "super + shift + {j,Down}" = "bspc-move-y ${amount}";
    "super + shift + {k,Up}" = "bspc-move-y -${amount}";
    "super + shift + {l,Right}" = "bspc-move-x ${amount}";

    # resize mode
    "super + r : {Left,h,Down,j,Up,k,Right,l,Return,space,r}" =
      concatStringsSep " \\\n  " [
        "{ bspc-resize-x -${amount}"
        ", bspc-resize-x -${amount}"
        ", bspc-resize-y ${amount}"
        ", bspc-resize-y ${amount}"
        ", bspc-resize-y -${amount}"
        ", bspc-resize-y -${amount}"
        ", bspc-resize-x ${amount}"
        ", bspc-resize-x ${amount}"
        ", ${xdotool} key Escape"
        ", ${xdotool} key Escape"
        ", ${xdotool} key Escape"
        "}"
      ];

    # focus workspace
    "super + {1-9,0}" = "bspc desktop -f '^{1-9,10}'";

    # move to workspace
    "super + shift + {1-9,0}" = "bspc node -d '^{1-9,10}'";
    };

  */

  amount =
    toString cfg.amount;

  focus = direction:
    ''
      bspc node 'focused.!floating' -f '${direction}.!floating'
      ||
      bspc node 'focused.floating' -f '${direction}.floating'
    '';

  move = direction:
    let
      prefix = "bspc node 'focused.!floating' --swap ${direction} ||";
    in
    if direction == "west" then
      "${prefix} bspc node 'focused.floating' --move -${amount} 0"
    else if direction == "south" then
      "${prefix} bspc node 'focused.floating' --move 0 ${amount}"

    else if direction == "north" then
      "${prefix} bspc node 'focused.floating' --move 0 -${amount}"

    else if direction == "east" then
      "${prefix} bspc node 'focused.floating' --move ${amount} 0"

    else
      builtins.throw "Invalid move direction";

  resize = vertical: grow:
    let
      dir = if vertical then "top" else "left";
      dir' = if vertical then "bottom" else "right";

      amount = value:
        let value' = toString (builtins.ceil value); in
        if vertical then "0 ${value'}" else "${value'} 0";

      n = if grow then (cfg.amount * -1) else cfg.amount;
    in
    ''
      bspc node 'focused.!floating' -z ${dir} ${amount n}
      ||
      bspc node 'focused.!floating' -z ${dir'} ${amount (n * -1)}
      ||
      bspc node 'focused.floating' -z ${dir} ${amount (n / 2)}
      ||
      bspc node 'focused.floating' -z ${dir'} ${amount (n / -2)}
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
      type = types.attrsOf (types.oneOf [ types.str (types.attrsOf types.str) ]);
      default = {
        # MISC
        ######## #### ## #

        # reload sxhkd configs
        "super + Escape" = "pkill -USR1 -x sxhkd";

        # panic restart bspwm
        "super + shift + r" = "bspc wm -r";

        # panic quit bspwm
        "super + shift + q" = "bspc quit";

        # WINDOW CONTROLS
        ######## #### ## #

        # close app
        "super + c" = "bspc node -c";

        # kill app
        "super + shift + c" = "bspc node -k";

        # toggle tiled state
        "super + f" = "bspc node -t ~tiled";

        # WINDOWING
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

        # RESIZE
        ######## #### ## #

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

            # "r" = escape;
            #"space" = escape;
            #"Return" = escape;
          };

        # WORKSPACES
        ######## #### ## #

        # focus
        "super + 1" = "bspc desktop -f '^1'";
        "super + 2" = "bspc desktop -f '^2'";
        "super + 3" = "bspc desktop -f '^3'";
        "super + 4" = "bspc desktop -f '^4'";
        "super + 5" = "bspc desktop -f '^5'";
        "super + 6" = "bspc desktop -f '^6'";
        "super + 7" = "bspc desktop -f '^7'";
        "super + 8" = "bspc desktop -f '^8'";
        "super + 9" = "bspc desktop -f '^9'";
        "super + 0" = "bspc desktop -f '^10'";

        # move
        "super + shift + 1" = "bspc node -d '^1'";
        "super + shift + 2" = "bspc node -d '^2'";
        "super + shift + 3" = "bspc node -d '^3'";
        "super + shift + 4" = "bspc node -d '^4'";
        "super + shift + 5" = "bspc node -d '^5'";
        "super + shift + 6" = "bspc node -d '^6'";
        "super + shift + 7" = "bspc node -d '^7'";
        "super + shift + 8" = "bspc node -d '^8'";
        "super + shift + 9" = "bspc node -d '^9'";
        "super + shift + 0" = "bspc node -d '^10'";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      #bspc-focus
      #bspc-move-x
      #bspc-move-y
      #bspc-resize-x
      #bspc-resize-y
      #bspc-toggle-focus
    ];

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
      bspc node -t {tiled,pseudo_tiled,floating,fullscreen}

  # set the node flags
  super + ctrl + {m,x,y,z}
      bspc node -g {marked,locked,sticky,private}

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
