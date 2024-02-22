{ config, lib, pkgs, specialArgs, ... }:
let
  inherit (config.modules.zutty) enable;
  inherit (config.modules.themes) color-scheme;
in
{
  options.modules.zutty.enable = lib.mkEnableOption "Enable zutty module";

  config = lib.mkIf enable {
    pacman.packages = [ "extra/zutty" ];

    xresources.properties = with color-scheme; {
      "Zutty.font" = "CaskaydiaCoveNerdFontMono";
      "Zutty.fontsize" = "16";
      "Zutty.boldColors" = "true";

      "Zutty.geometry" = "80x24";

      "Zutty.saveLines" = "50000";
      "Zutty.shell" = "fish";

      "Zutty.bg" = "${background}";
      "Zutty.fg" = "${foreground}";

      "Zutty.cr" = "${foregroundBright}";

      "Zutty.color0" = "${black}";
      "Zutty.color8" = "${blackBright}";
      "Zutty.color1" = "${red}";
      "Zutty.color9" = "${redBright}";
      "Zutty.color2" = "${green}";
      "Zutty.color10" = "${greenBright}";
      "Zutty.color3" = "${yellow}";
      "Zutty.color11" = "${yellowBright}";
      "Zutty.color4" = "${blue}";
      "Zutty.color12" = "${blueBright}";
      "Zutty.color5" = "${magenta}";
      "Zutty.color13" = "${magentaBright}";
      "Zutty.color6" = "${cyan}";
      "Zutty.color14" = "${cyanBright}";
      "Zutty.color7" = "${white}";
      "Zutty.color15" = "${whiteBright}";
    };
  };
}
