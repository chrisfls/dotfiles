{ config, lib, pkgs, specialArgs, ... }:
let inherit (config.presets) desktop; in {
  options.presets.desktop = lib.mkEnableOption "Enable desktop preset";

  config = lib.mkIf desktop {
    pacman.explicits = [
      "extra/lostfiles"
      "aur/sublime-merge"
      "aur/sublime-text-4"
      "aur/audio-share-bin"
    ];

    pacman.packages = [
      # ######## #### ## #
      # DESK ENV
      # ######## #### ## #

      # cli
      "extra/alsa-utils"
      "extra/pamixer"

      # dialogs
      "extra/kdialog"
      "extra/lxqt-openssh-askpass"
      "extra/lxqt-policykit"
      "extra/lxqt-sudo"

      # misc
      "chaotic-aur/nm-tray"

      # core apps
      "extra/lxqt-archiver"
      "chaotic-aur/bottles" # TODO: replace?
      "chaotic-aur/joycond-git"
      "extra/arandr" # TODO: replace
      "extra/cameractrls"
      "extra/pavucontrol-qt"
      "extra/dolphin"

      # common apps
      "extra/haruna"
      "extra/audacity"
      "extra/copyq"
      "extra/inkscape"
      "extra/featherpad"
      "extra/juk"
      "extra/kolourpaint"
      "extra/qalculate-qt"
      "extra/qps"

      # ######## #### ## #
      # MY PKGS
      # ######## #### ## #

      "chaotic-aur/anydesk-bin"
      "chaotic-aur/logseq-desktop-bin"
      "chaotic-aur/moonlight-qt"
      "extra/obsidian"
      "chaotic-aur/parsec-bin"
      "extra/qbittorrent"
      "chaotic-aur/webcord"
    ];

    modules = {
      autorandr.enable = true;
      brave.enable = true;
      dunst.enable = true;
      edge.enable = true;
      feh.enable = true;
      fonts.enable = true;
      i3wm = {
        enable = true;

        startup = [
          "copyq"
          "nm-tray"
          "webcord --start-minimized"
        ];

        apps = {
          "c" = "io.github.Qalculate.qalculate-qt";
          "d" = "webcord";
          "e" = "org.kde.dolphin";
          "shift+c" = "com.github.hluk.copyq";
          "w" = "com.github.eneshecan.WhatsAppForLinux";
        };

        extraConfig = "bindsym Control+Mod1+Delete exec --no-startup-id gtk-launch qps";
      };
      sway = {
        enable = true;

        startup = [
          "copyq"
          "nm-tray"
          "webcord --start-minimized"
        ];

        apps = {
          "c" = "io.github.Qalculate.qalculate-qt";
          "d" = "webcord";
          "e" = "org.kde.dolphin";
          "shift+c" = "com.github.hluk.copyq";
          "w" = "com.github.eneshecan.WhatsAppForLinux";
        };

        extraConfig = "bindsym Control+Mod1+Delete exec --no-startup-id gtk-launch qps";
      };
      jamesdsp.enable = true;
      kitty.enable = true;
      loopback-toggle.enable = true;
      micro = { enable = true; desktop = true; };
      picom.enable = true;
      polybar.enable = true;
      qview.enable = true;
      rclone.enable = true;
      rofi.enable = true;
      screenshot.enable = true;
      steam.enable = true;
      systemd.imported-variables = [ "PATH" ];
      telegram.enable = true;
      theme.enable = true;
      udiskie.enable = true;
      waybar.enable = true;
      xdg.enable = true;
      xorg.enable = true;
    };

    xdg.configFile."kwalletrc".text =
      ''
        [Wallet]
        Enabled=false
      '';
  };
}
