{ config, lib, pkgs, specialArgs, ... }:
let
  inherit (config.presets) non-nixos;
  inherit (config.modules.kitty) enable;
  inherit (config.modules.themes) color-scheme;
in
{
  options.modules.kitty.enable = lib.mkEnableOption "Enable kitty module";

  config = lib.mkIf enable {
    home.packages = lib.mkIf (!non-nixos) [ pkgs.kitty ];

    pacman.packages = [ "extra/kitty" ];

    modules.i3wm.terminal = "kitty -1";

    xdg.configFile."kitty/kitty.conf".text =
      with config.modules.themes.color-scheme;
      ''
        font_family CaskaydiaCove NFM
        font_size 16.0

        remember_window_size no
        initial_window_width 80c
        initial_window_height 24c

        scrollback_lines 1000000
        editor micro
        shell fish

        background ${background}
        background_opacity 0.9
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
