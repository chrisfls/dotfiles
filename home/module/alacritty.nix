{ config, lib, pkgs, specialArgs, ... }:
let
  cfg = config.extra.alacritty;

  theme = specialArgs.color-schemes.popping-and-locking-black;
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
        shell.program = "${pkgs.fish}/bin/fish";
        colors = {
          primary = {
            background = theme.background;
            foreground = theme.foreground;
          };
          normal = {
            black = theme.black;
            blue = theme.blue;
            cyan = theme.cyan;
            green = theme.green;
            magenta = theme.magenta;
            red = theme.red;
            white = theme.white;
            yellow = theme.yellow;
          };
          bright = {
            black = theme.blackBright;
            blue = theme.blueBright;
            cyan = theme.cyanBright;
            green = theme.greenBright;
            magenta = theme.magentaBright;
            red = theme.redBright;
            white = theme.whiteBright;
            yellow = theme.yellowBright;
          };
        };
      };
    };
  };
}
