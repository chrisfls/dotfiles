{ config, lib, pkgs, ... }:
let inherit (config.modules.helix) enable; in {
  options.modules.helix.enable = lib.mkEnableOption "Enable helix module";

  config = lib.mkIf enable {
    programs.helix = {
      enable = true;
      extraPackages = [ ];
      settings = {
        theme = "gruvbox_dark_hard";
        editor = {
          bufferline = "multiple";
          default-line-ending = "lf";
          line-number = "relative";
          rulers = [ 79 99 119 ];
          text-width = 120;
          color-modes = true;
          cursorline = true;
          true-color = true;
          cursor-shape = {
            insert = "bar";
            normal = "underline";
            select = "block";
          };
          indent-guides = {
            render = true;
            character = "╎";
            skip-levels = 1;
          };
        };
      };
    };
  };
}
