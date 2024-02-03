{ config, lib, pkgs, specialArgs, ... }:
let
  inherit (config.presets) desktop development archlinux;
  enable = desktop && development;
in
lib.mkIf enable {
  modules = {
    code.enable = true;
    emacs.enable = true;
  };

  home.packages = [
    # trying because I'm curious
    pkgs.tig

    # eval which I'll use
    pkgs.gitui
    pkgs.lazygit
  ] ++ (if archlinux then [ ] else [
    pkgs.dbeaver
    pkgs.gittyup
    pkgs.github-desktop
  ]);

  pacman.packages = [
    "extra/dbeaver"
    "chaotic-aur/gittyup"
    "chaotic-aur/github-desktop"
  ];
}
