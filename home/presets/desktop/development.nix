{ config, lib, pkgs, specialArgs, ... }:
let
  inherit (config.presets) desktop development;
  enable = desktop && development;
in
lib.mkIf enable {
  modules = {
    code.enable = true;
    helix.enable = true;
    emacs.enable = true;
    lite-xl.enable = true;
  };

  home.packages = [
    pkgs.dbeaver
    pkgs.gittyup

    # trying because I'm curious
    pkgs.github-desktop
    pkgs.tig

    # eval which I'll use
    pkgs.gitui
    pkgs.lazygit
  ];
}
