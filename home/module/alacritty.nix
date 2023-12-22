{ config, lib, pkgs, specialArgs, ... }:
let
  cfg = config.extra.alacritty;

  colors = config.extra.themes.color-scheme;
in
{
  options.extra.alacritty.enable = lib.mkEnableOption "Enable alacritty module";

  config = lib.mkIf cfg.enable {
    services.sxhkd.keybindings = {
      "super + semicolon" = "alacritty";
      "super + BackSpace" = "alacritty";
    };

    programs.alacritty = {
      enable = true;
      settings = {
        # TODO: default font
        shell.program = "${pkgs.fish}/bin/fish";
        colors = {
          primary = {
            background = colors.background;
            foreground = colors.foreground;
          };
          normal = {
            black = colors.black;
            blue = colors.blue;
            cyan = colors.cyan;
            green = colors.green;
            magenta = colors.magenta;
            red = colors.red;
            white = colors.white;
            yellow = colors.yellow;
          };
          bright = {
            black = colors.blackBright;
            blue = colors.blueBright;
            cyan = colors.cyanBright;
            green = colors.greenBright;
            magenta = colors.magentaBright;
            red = colors.redBright;
            white = colors.whiteBright;
            yellow = colors.yellowBright;
          };
          # TOOD: dim colors
        };
      };
    };
  };
}
