{ config, pkgs, specialArgs, ... }: with specialArgs;
{
  home.username = ssot.users.arch-rmxp.kress.username;
  home.homeDirectory = ssot.users.arch-rmxp.kress.home;

  # before changing this value read the documentation for this option
  home.stateVersion = "23.11";

  programs.waybar.settings.mainBar.output = [ "HDMI-A-1" ];

  xdg.configFile."hypr/hyprland.conf".text = ''
    monitor=eDP-1,disable
    monitor=HDMI-A-1,preferred,auto,auto

    env = GDK_SCALE,1.5
    env = XCURSOR_SIZE,16
    env = NIXOS_OZONE_WL,1
  '';

  # let home manager install and manage itself
  programs.home-manager.enable = true;
}
