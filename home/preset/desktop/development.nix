{ config, lib, pkgs, specialArgs, ... }:
let
  inherit (config.preset) desktop development;
  enable = desktop && development;
in
lib.mkIf enable {
  module = {
    code.enable = true;
    sublime.enable = true;
    # helix.enable = true;
    # emacs.enable = true;
  };
}
