{ config, lib, pkgs, ... }:
let inherit (config.module.fonts) enable; in {
  options.module.fonts.enable = lib.mkEnableOption "Enable fonts module";

  config.home.packages = lib.mkIf enable [
    # good generic font
    pkgs.noto-fonts

    # good emoji font
    pkgs.noto-fonts-color-emoji

    # good heading fonts
    pkgs.overpass
    pkgs.montserrat

    # good terminal fonts
    (pkgs.nerdfonts.override { fonts = [ "JetBrainsMono" "CascadiaCode" ]; })

    # good programming fonts
    pkgs.jetbrains-mono
    pkgs.cascadia-code
  ];
}
