{ config, lib, pkgs, specialArgs, ... }:
let
  cfg = config.module.alacritty;

  colors = config.module.themes.color-scheme;
in
{
  options.module.alacritty.enable = lib.mkEnableOption "Enable alacritty module";

  config = lib.mkIf cfg.enable {
    module.sxhkd.keybindings =
      let
        cmd =
          ''
            dir=$(${pkgs.xcwd}/bin/xcwd);
            if [ "$dir" = "${config.home.homeDirectory}" ]; then
              alacritty;
            else
              alacritty --working-directory "$dir";
            fi;
          '';
      in
      {
        "super + semicolon" = cmd;
        "super + BackSpace" = cmd;
      };

    programs.alacritty = {
      enable = true;
      settings = {
        shell.program = "${pkgs.fish}/bin/fish";
        working_directory = config.xdg.userDirs.desktop;
        window = {
          opacity = 0.9;
          padding = {
            x = 8;
            y = 8;
          };
          dimensions = {
            columns = 80;
            lines = 24;
          };
        };
        scrolling = {
          history = 100000;
          multiplier = 3;
        };
        font = {
          family = "CaskaydiaCove NFM";
          size = 12.0;
        };
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
          dim = {
            black = colors.blackDim;
            blue = colors.blueDim;
            cyan = colors.cyanDim;
            green = colors.greenDim;
            magenta = colors.magentaDim;
            red = colors.redDim;
            white = colors.whiteDim;
            yellow = colors.yellowDim;
          };
        };
      };
    };
  };
}
