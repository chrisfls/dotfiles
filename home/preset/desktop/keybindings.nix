{ config, pkgs, ... }:
let
  move = dir: x: y:
    let
      dx = toString x;
      dy = toString y;
    in
    "bspc node --move ${dx} ${dy} || bspc node --swap ${dir}";
  amount = 20;
in
{
  home.packages = [ pkgs.xdotool ];

  services.sxhkd = {
    enable = true;
    keybindings = {
      # MISC
      ######## #### ## #

      # reload config
      "super + Escape" = "pkill -USR1 -x sxhkd";

      # restart / quit
      "super + shift + {r, q }" = "bspc {wm -r,quit}";

      # logout
      # "super + shift + e" = "";

      # WINDOW CONTROLS
      ######## #### ## #

      # close / kill
      "super + {_,shift +} c" =
        "bspc node -{c,k}";

      # toggle tiled / monocle / fullscreen
      "super + {_,shift +} m" =
        "bspc {desktop -l next,node -t fullscreen}";

      # tiled / pseudo tiled
      "super + s" =
        "bspc node -t ~tiled";
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

      #"super + space" =
      #  "bspc node -t floating";
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
      "super + {h,j,k,l,Left,Down,Up,Right}" =
        "bspc node -f {west,south,north,east,west,south,north,east}";

      # move windows
      "super + shift + {h,Left}" =
        move "west" (- amount) 0;
      "super + shift + {j,Down}" =
        move "south" 0 amount;
      "super + shift + {k,Up}" =
        move "north" 0 (-amount);
      "super + shift + {l,Right}" =
        move "east" amount 0;

      # resize windows
      "super + r : {h,j,k,l,Left,Down,Up,Right,Return,super + s}" =
        ''V = 20; \
            { bspc node -z left   -$V   0 \
            , bspc node -z bottom   0  $V \
            , bspc node -z top      0 -$V \
            , bspc node -z right   $V   0 \
            , bspc node -z left   -$V   0 \
            , bspc node -z bottom   0  $V \
            , bspc node -z top      0 -$V \
            , bspc node -z right   $V   0 \
            , xdotool key Escape \
            , xdotool key Escape \
            }
        '';

      # WORKSPACES
      ######## #### ## #

      # focus / move
      "super + {_,shift +} {1-9,0}" = "bspc {desktop -f,node -d} '^{1-9,10}'";

    };
  };
}

# SAMPLE BSPWM configs:
/*
  #
  # wm independent hotkeys
  #

  # terminal emulator
  super + Return
   	urxvt

  # program launcher
  super + @space
   	dmenu_run


  #
  # bspwm hotkeys
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
