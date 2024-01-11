{ config, lib, pkgs, specialArgs, ... }:
let
  inherit (config.presets) desktop development;
  enable = desktop && development;
in
lib.mkIf enable {
  modules = {
    code.enable = true;
    sublime.enable = true;
    helix.enable = true;
    emacs.enable = true;
  };

  home.packages = [ pkgs.dbeaver ];
}
