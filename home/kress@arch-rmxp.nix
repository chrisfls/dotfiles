{ config, pkgs, specialArgs, ... }: with specialArgs;
{
  home.username = ssot.users.arch-rmxp.kress.username;
  home.homeDirectory = ssot.users.arch-rmxp.kress.home;

  # before changing this value read the documentation for this option
  home.stateVersion = "23.05";

  # let home manager install and manage itself
  programs.home-manager.enable = true;
}
