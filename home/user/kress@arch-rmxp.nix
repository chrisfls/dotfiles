{ config, pkgs, specialArgs, ... }:
let
  ssot = specialArgs.ssot;
in
{
  imports = [
    ../preset/non-nixos.nix
    ../preset/development.nix
    ../preset/desktop.nix
    ../preset/work.nix
    ../preset/gamedev.nix
    ../cloudflare-warp.nix
  ];

  /*
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
  */


  home.username = ssot.users.arch-rmxp.kress.username;
  home.homeDirectory = ssot.users.arch-rmxp.kress.home;

  # before changing this value read the documentation for this option
  home.stateVersion = "23.11";

  # let home manager install and manage itself
  programs.home-manager.enable = true;
}
