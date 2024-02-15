{ config, lib, pkgs, ... }:
let inherit (config.modules.fonts) enable; in {
  options.modules.fonts.enable = lib.mkEnableOption "Enable fonts module";

  config = lib.mkIf enable {
    pacman.packages = [
      "extra/noto-fonts"
      "extra/noto-fonts-emoji"
      "extra/ttf-overpass"
      "extra/ttf-montserrat"
      "extra/ttf-cascadia-code"
      "extra/ttf-cascadia-code-nerd"
      "extra/ttf-jetbrains-mono"
      "extra/ttf-jetbrains-mono-nerd"
      "extra/ttf-liberation"
    ];
  };
}
