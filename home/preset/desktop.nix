{ config, lib, pkgs, ... }:
let
  lxqt = pkgs.lxqt;
in
{
  imports = [
    ../i3wm.nix
    ../lxqt.nix
    ./desktop/theme.nix
  ];

  config = {
    home.packages = [
      pkgs.alsa-utils
      pkgs.pamixer

      # management
      pkgs.arandr

      # browser
      pkgs.brave
    ];

    xsession.windowManager.i3 = {
      enable = true;
      config = {
        menu = "rofi -show drun";
        terminal = "nixGLIntel wezterm";
      };
    };

    programs.rofi = {
      enable = true;
    };

    programs.wezterm = {
      # maybe migrate to foot after https://codeberg.org/dnkl/foot/issues/57
      # unless no issues with gpu accel are found
      enable = true;
    };


  };
}
