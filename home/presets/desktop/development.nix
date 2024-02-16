{ config, lib, pkgs, specialArgs, ... }:
let
  inherit (config.presets) desktop development;
  enable = desktop && development;
in
lib.mkIf enable {
  modules = {
    code.enable = true;
    emacs.enable = true;
  };

  # TODO: trying because I'm curious
  # home.packages = [
  #   pkgs.tig

  #   # TODO: eval which I'll use
  #   pkgs.gitui
  #   pkgs.lazygit
  # ];

  pacman.packages = [
    "extra/dbeaver"
    "chaotic-aur/gittyup"
    "chaotic-aur/github-desktop"
  ];
}
