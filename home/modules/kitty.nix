{ config, lib, pkgs, specialArgs, ... }:
let
  inherit (config.modules.kitty) enable;
  inherit (config.modules.themes) color-scheme;
  inherit (specialArgs) mkIfElse;

  kitty = "${config.programs.kitty.package}/bin/kitty";

  launch = "exec --no-startup-id \"kitty -1\"";
in
{
  options.modules.kitty.enable = lib.mkEnableOption "Enable kitty module";

  config = lib.mkIf enable {
    xsession.windowManager.i3.config.terminal = launch;

    programs.kitty = {
      enable = true;
      settings = with config.modules.themes.color-scheme; {
        font_family = "CaskaydiaCove NFM";
        font_size = "12.0";

        remember_window_size = "no";
        initial_window_width = "80c";
        initial_window_height = "24c";

        scrollback_lines = "1000000";
        editor = "micro";
        shell = "fish";

        background = background;
        background_opacity = "0.9";
        foreground = foreground;

        cursor = foregroundBright;
        selection_foreground = "none";
        selection_background = foregroundDim;

        color0 = black;
        color8 = blackBright;
        color1 = red;
        color9 = redBright;
        color2 = green;
        color10 = greenBright;
        color3 = yellow;
        color11 = yellowBright;
        color4 = blue;
        color12 = blueBright;
        color5 = magenta;
        color13 = magentaBright;
        color6 = cyan;
        color14 = cyanBright;
        color7 = white;
        color15 = whiteBright;
      };
    };
  };
}
