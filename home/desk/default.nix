{ config, lib, pkgs, specialArgs, ... }:
with specialArgs;
{
  imports = [
    ./waybar/waybar.nix
    ./hypr/hyprland.nix
    ./kitty/kitty.nix
  ];

  theme = themes.popping-and-locking;

  fonts.fontconfig.enable = true;

  services.easyeffects.enable = true;

  xdg.enable = true;

  home.packages = with pkgs; [
    rofi-wayland
    hyprpaper
    networkmanagerapplet

    wl-clipboard

    # screenshot
    grimblast

    dunst # notifications
    pamixer # used by bar

    libsForQt5.audiotube

    languagetool
    xdg-desktop-portal-hyprland
    webcord-vencord
    alsa-utils
    lxqt.pcmanfm-qt
    lxqt.lxqt-sudo
    lxqt.lxqt-sudo
    lxqt.lxqt-notificationd
    lxqt.lxqt-archiver
    lxqt.lximage-qt
    lxqt.qps
    lxqt.screengrab
    lxqt.pavucontrol-qt
    lxqt.lxqt-powermanagement
    lxqt.lxqt-policykit
    lxqt.lxqt-openssh-askpass
  ];


  qt = {
    enable = true;
    platformTheme = "gtk";
    style.name = "breeze";
  };

  /*
    programs.bash = {
    sessionVariables = {
      XDG_CONFIG_HOME = "$HOME/.config";
      XDG_DATA_HOME = "$HOME/.local/share";
      XDG_CACHE_HOME = "$HOME/.cache";
    };
    };
  */

  # TODO: configure
  services.dunst = {
    # TODO: avizo or dunstify as volume/brightness level indicator or 
    enable = true;
    iconTheme = {
      name = "Adwaita";
      package = pkgs.gnome.adwaita-icon-theme;
      size = "32x32";
    };
    settings = {
      global = {
        width = 300;
        height = 300;
        offset = "30x50";
        origin = "top-right";
        transparency = 10;
        frame_color = "#eceff1";
        font = "Droid Sans 9";
      };

      urgency_normal = {
        background = "#37474f";
        foreground = "#eceff1";
        timeout = 10;
      };
    };
  };
}
