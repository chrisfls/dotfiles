{ config, lib, pkgs, ... }:
let inherit (config.modules.fonts) enable; in {
  options.modules.fonts.enable = lib.mkEnableOption "Enable fonts module";

  config = lib.mkIf enable {
    pacman.packages = [
      "extra/adobe-source-code-pro-fonts"
      "extra/adobe-source-han-sans-cn-fonts"
      "extra/adobe-source-han-sans-hk-fonts"
      "extra/adobe-source-han-sans-jp-fonts"
      "extra/adobe-source-han-sans-kr-fonts"
      "extra/adobe-source-han-sans-otc-fonts"
      "extra/adobe-source-han-sans-tw-fonts"
      "extra/adobe-source-han-serif-cn-fonts"
      "extra/adobe-source-han-serif-hk-fonts"
      "extra/adobe-source-han-serif-jp-fonts"
      "extra/adobe-source-han-serif-kr-fonts"
      "extra/adobe-source-han-serif-otc-fonts"
      "extra/adobe-source-han-serif-tw-fonts"
      "extra/adobe-source-sans-fonts"
      "extra/adobe-source-serif-fonts"
      "extra/cantarell-fonts"
      "extra/noto-fonts-cjk"
      "extra/noto-fonts-emoji"
      "extra/noto-fonts-extra"
      "extra/noto-fonts"
      "extra/otf-fira-sans"
      "extra/ttf-cascadia-code-nerd"
      "extra/ttf-cascadia-code"
      "extra/ttf-croscore"
      "extra/ttf-dejavu"
      "extra/ttf-droid"
      "extra/ttf-jetbrains-mono-nerd"
      "extra/ttf-jetbrains-mono"
      "extra/ttf-liberation"
      "extra/ttf-montserrat"
      "extra/ttf-noto-nerd"
      "extra/ttf-overpass"
      "extra/ttf-roboto"
    ];
  };
}
