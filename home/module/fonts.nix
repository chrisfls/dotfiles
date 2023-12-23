{ config, lib, pkgs, ... }:
let
  cfg = config.module.fonts;
in
{
  options.module.fonts.enable = lib.mkEnableOption "Enable fonts module";

  config = lib.mkIf cfg.enable {
    home.packages = [
      # good heading fonts
      pkgs.overpass
      pkgs.montserrat

      # terminal fonts
      (pkgs.nerdfonts.override { fonts = [ "JetBrainsMono" "CascadiaCode" ]; })

      # programming fonts
      pkgs.jetbrains-mono
      pkgs.cascadia-code

      # emoji fonts
      pkgs.noto-fonts-color-emoji
    ];
  };
}
