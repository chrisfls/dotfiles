{ config, lib, pkgs, specialArgs, ... }:
let inherit (config.modules.kitty) enable color-scheme; in {
  options.modules.kitty = {
    enable = lib.mkEnableOption "Enable kitty module";

    color-scheme = lib.mkOption {
      type = lib.types.attrs;
      default = specialArgs.color-schemes.breeze;
    };
  };

  config = lib.mkIf enable {
    pacman.packages = [ "extra/kitty" ];

    xdg.configFile."kitty/kitty.conf".text =
      with color-scheme;
      ''
        editor micro
        shell fish
        
        repaint_delay 16
        input_delay 1
        scrollback_lines 1000000
      font_family CaskaydiaCove NFM
        font_size 10.0
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
        background_opacity 1
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