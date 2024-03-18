{ config, lib, pkgs, specialArgs, ... }:
let
  inherit (config.presets) desktop development;
  enable = desktop && development;
in
lib.mkIf enable {
  pacman.packages = [
    "extra/dbeaver"
    "chaotic-aur/gittyup"
    "chaotic-aur/github-desktop"
  ];

  modules = {
    code.enable = true;
    emacs.enable = true;
  };
}
