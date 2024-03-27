{ config, lib, pkgs, ... }:
let inherit (config.modules.helix) enable; in {
  options.modules.helix.enable = lib.mkEnableOption "Enable helix module";

  config = lib.mkIf enable {
    pacman.packages = [ "chaotic-aur/helix-git" ];

    xdg.configFile."helix/config.toml".text =
      ''
        theme = "gruvbox_dark_hard"
        [editor]
        bufferline = "multiple"
        color-modes = true
        cursorline = true
        default-line-ending = "lf"
        line-number = "relative"
        rulers = [79, 99, 119]
        text-width = 120
        true-color = true

        [editor.cursor-shape]
        insert = "bar"
        normal = "underline"
        select = "block"

        [editor.indent-guides]
        character = "╎"
        render = true
        skip-levels = 1
      '';
  };
}
