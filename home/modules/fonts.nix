{ config, lib, pkgs, ... }:
let
  inherit (config.presets) non-nixos;
  inherit (config.modules.fonts) enable;
in
{
  options.modules.fonts.enable = lib.mkEnableOption "Enable fonts module";

  config = lib.mkIf enable {
    home.packages =
      if non-nixos then [ ]
      else [
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

    pacman.packages = [
      "extra/noto-fonts"
      "extra/noto-fonts-emoji"
      "extra/ttf-overpass"
      "extra/ttf-montserrat"
      "extra/ttf-cascadia-code"
      "extra/ttf-cascadia-code-nerd"
      "extra/ttf-jetbrains-mono"
      "extra/ttf-jetbrains-mono-nerd"
    ];
  };
}
