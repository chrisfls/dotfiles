{ config, lib, pkgs, ... }:
let inherit (config.module.fonts) enable; in {
  options.module.fonts.enable = lib.mkEnableOption "Enable fonts module";

  config = lib.mkIf enable {
    pacman.packages = lib.mkIf config.preset.non-nixos [
      "extra/noto-fonts"
      "extra/noto-fonts-emoji"

      "extra/ttf-montserrat"
      "extra/ttf-overpass"

      "extra/ttf-cascadia-code-nerd"
      "extra/ttf-cascadia-code"

      "extra/ttf-jetbrains-mono-nerd"
      "extra/ttf-jetbrains-mono"
    ];

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
