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

    exec-once = hyprctl setcursor breeze_cursors 24pt
  '';

  homeage = {
    identityPaths = [ ".ssh/id_ed25519" ];
    installationType = "systemd";
  };

  programs.bash = {
    sessionVariables = {
      ELM_SCALE = 1;
      GDK_SCALE = 1;
      NIXOS_OZONE_WL = 1;
      QT_SCALE_FACTOR = 1;
      XCURSOR_THEME = "breeze_cursors";
      XCURSOR_SIZE = 48;
    };
  };

  # let home manager install and manage itself
  programs.home-manager.enable = true;
}
