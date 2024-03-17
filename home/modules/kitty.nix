# evaluate migrating to Zutty
{ config, lib, pkgs, specialArgs, ... }:
let
  inherit (config.modules.kitty) enable;
  inherit (config.modules.themes) color-scheme;
in
{
  options.modules.kitty.enable = lib.mkEnableOption "Enable kitty module";

  config = lib.mkIf enable {
    pacman.packages = [ "extra/kitty" ];

    modules = {
      i3wm.terminal = "kitty -1";
      sway.terminal = "kitty -1";
    };

    xdg.configFile."kitty/kitty.conf".text =
      with config.modules.themes.color-scheme;
      ''
        editor micro
        shell fish
        
        repaint_delay 16
        input_delay 1
        scrollback_lines 1000000

        font_family CaskaydiaCove NFM
        font_size 16.0

        tab_bar_edge top
        tab_bar_align left
        tab_bar_style powerline
        tab_powerline_style round
        
        tab_bar_background ${blackDim}
        tab_bar_margin_color ${blackDim}

        active_tab_foreground   ${background}
        active_tab_background   ${foreground}
        active_tab_font_style   bold
        inactive_tab_foreground ${background}
        inactive_tab_background ${blackBright}
        inactive_tab_font_style normal

        remember_window_size no
        initial_window_width 80c
        initial_window_height 24c

        background ${background}
        background_opacity 0.80
        foreground ${foreground}

        cursor ${foregroundBright}
        selection_foreground none
        selection_background ${foregroundDim}


        color0 ${black}
        color8 ${blackBright}
        color1 ${red}
        color9 ${redBright}
        color2 ${green}
        color10 ${greenBright}
        color3 ${yellow}
        color11 ${yellowBright}
        color4 ${blue}
        color12 ${blueBright}
        color5 ${magenta}
        color13 ${magentaBright}
        color6 ${cyan}
        color14 ${cyanBright}
        color7 ${white}
        color15 ${whiteBright}
      '';
  };
}
# IR_Black
/*
  background            #000000
  foreground            #f1f1f1
  cursor                #7f7f7f
  selection_background  #b4d5ff
  color0                #4f4f4f
  color8                #7b7b7b
  color1                #fa6c5f
  color9                #fcb6af
  color2                #a8fe60
  color10               #ceffab
  color3                #fffeb6
  color11               #fffecc
  color4                #96cafd
  color12               #b5dcfe
  color5                #fa72fc
  color13               #fb9bfe
  color6                #c6c4fd
  color14               #dfdffd
  color7                #eeedee
  color15               #fefffe
  selection_foreground #000000
*/

# Tango_Dark
/*
  background            #000000
  foreground            #ffffff
  cursor                #ffffff
  selection_background  #b4d5ff
  color0                #000000
  color8                #545753
  color1                #cc0000
  color9                #ef2828
  color2                #4e9a05
  color10               #8ae234
  color3                #c4a000
  color11               #fce94e
  color4                #3464a4
  color12               #719ecf
  color5                #74507a
  color13               #ad7ea7
  color6                #05989a
  color14               #34e2e2
  color7                #d3d7cf
  color15               #ededec
  selection_foreground #000000
*/

# Symfonic
/*
  background            #000000
  foreground            #ffffff
  cursor                #dc322f
  selection_background  #073642
  color0                #000000
  color8                #1b1d21
  color1                #dc322f
  color9                #dc322f
  color2                #56db3a
  color10               #56db3a
  color3                #ff8400
  color11               #ff8400
  color4                #0084d4
  color12               #0084d4
  color5                #b729d9
  color13               #b729d9
  color6                #ccccff
  color14               #ccccff
  color7                #ffffff
  color15               #ffffff
  selection_foreground #000000
*/
# Pro
/*
  # Theme ported from the Mac Terminal application.
  background            #000000
  foreground            #f2f2f2
  cursor                #4d4d4d
  selection_background  #414141
  color0                #000000
  color8                #666666
  color1                #990000
  color9                #e50000
  color2                #00a600
  color10               #00d900
  color3                #999900
  color11               #e5e500
  color4                #1f08db
  color12               #0000ff
  color5                #b200b2
  color13               #e500e5
  color6                #00a6b2
  color14               #00e5e5
  color7                #bfbfbf
  color15               #e5e5e5
  selection_foreground #000000
*/

# PaulMillr
/*
  background            #000000
  foreground            #f1f1f1
  cursor                #4c4c4c
  selection_background  #414141
  color0                #2a2a2a
  color8                #666666
  color1                #ff0000
  color9                #ff007f
  color2                #78ff0e
  color10               #66ff66
  color3                #e6be00
  color11               #f3d64d
  color4                #396ad6
  color12               #7099ec
  color5                #b348bd
  color13               #da66e5
  color6                #66ccff
  color14               #79def1
  color7                #bababa
  color15               #ffffff
  selection_foreground #000000
*/

# Dark_Pastel
/*
  background            #000000
  foreground            #ffffff
  cursor                #bbbbbb
  selection_background  #b5d5ff
  color0                #000000
  color8                #545454
  color1                #ff5555
  color9                #ff5555
  color2                #55ff55
  color10               #55ff55
  color3                #ffff55
  color11               #ffff55
  color4                #5555ff
  color12               #5555ff
  color5                #ff55ff
  color13               #ff55ff
  color6                #55ffff
  color14               #55ffff
  color7                #bbbbbb
  color15               #ffffff
  selection_foreground #000000
*/
