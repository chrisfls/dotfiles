{ config, lib, pkgs, ... }:
let inherit (config.module.fonts) enable; in {
  options.module.fonts.enable = lib.mkEnableOption "Enable fonts module";

  config = lib.mkIf enable {
    home.packages = [
      # good generic font
      pkgs.noto-fonts

      # good emoji font
      pkgs.noto-fonts-color-emoji

      # good heading fonts
      pkgs.overpass
      pkgs.montserrat

      # good terminal fonts
      (pkgs.nerdfonts.override { fonts = [ "CascadiaCode" "JetBrainsMono" ]; })

      # good programming fonts
      pkgs.cascadia-code
      pkgs.jetbrains-mono
    ];
  };
}
