{ config, lib, pkgs, ... }:
let inherit (config.module.fonts) enable; in {
  options.module.fonts.enable = lib.mkEnableOption "Enable fonts module";

  config = lib.mkIf enable {
    pacman.usr = {
      noto-fonts = [ "extra/noto-fonts" ];
      noto-fonts-color-emoji = [ "extra/noto-fonts-emoji" "chaotic-aur/noto-color-emoji-fontconfig" ];
      overpass = [ "extra/otf-overpass" ];
      montserrat = [ "extra/otf-montserrat" ];
      jetbrains-mono = [ "extra/ttf-jetbrains-mono" ];
      cascadia-code = [ "extra/ttf-cascadia-code" ];
    };

    pacman.nix.nerdfonts = [
      "extra/ttf-cascadia-code-nerd"
      "extra/ttf-cascadia-mono-nerd"
      "extra/ttf-jetbrains-mono-nerd"
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
      (pkgs.nerdfonts.override { fonts = [ "JetBrainsMono" "CascadiaCode" ]; })

      # good programming fonts
      pkgs.jetbrains-mono
      pkgs.cascadia-code
    ];
  };
}
