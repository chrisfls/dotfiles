{ config, pkgs, specialArgs, ... }: with specialArgs;
{
  home.username = ssot.users.arch-rmxp.kress.username;
  home.homeDirectory = ssot.users.arch-rmxp.kress.home;

  # before changing this value read the documentation for this option
  home.stateVersion = "23.11";

  programs.waybar.settings.mainBar.output = [ "HDMI-A-1" ];

  # let home manager install and manage itself
  programs.home-manager.enable = true;
}
