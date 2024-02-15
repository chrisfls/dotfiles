{ config, lib, pkgs, specialArgs, ... }:
let inherit (config.presets) desktop; in {
  options.presets.desktop = lib.mkEnableOption "Enable desktop preset";

  config = lib.mkIf desktop {
    modules = {
      autorandr.enable = true;
      brave.enable = true;
      dunst.enable = true;
      feh.enable = true;
      fontconfig.enable = true;
      fonts.enable = true;
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
      telegram.enable = true;
      themes.enable = true;
      udiskie.enable = true;
      xdg.enable = true;
      xorg = {
        enable = true;
        imported-variables = [ "PATH" ];
      };
      i3wm = {
        enable = true;

        startup = [
          "copyq"
          "nm-tray"
          "whatsapp-for-linux"
          "webcord --start-minimized"
        ];

        apps = {
          "c" = "io.github.Qalculate.qalculate-qt";
          "d" = "webcord";
          "e" = "pcmanfm-qt";
          "shift+c" = "com.github.hluk.copyq";
          "w" = "com.github.eneshecan.WhatsAppForLinux";
        };

        extraConfig = "bindsym Control+Mod1+Delete exec --no-startup-id gtk-launch qps";
      };
    };

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
      "extra/pavucontrol-qt"
      "extra/pcmanfm-qt"

      # common apps
      "extra/copyq"
      "extra/featherpad"
      "extra/juk"
      "extra/kolourpaint"
      "extra/qps"
      "chaotic-aur/mpc-qt"
      "extra/qalculate-qt"

      # themes
      "extra/papirus-icon-theme"
      "extra/materia-kde"
      "extra/materia-gtk-theme"

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
      "chaotic-aur/whatsapp-for-linux"
    ];

    xdg.configFile."kwalletrc".text =
      ''
        [Wallet]
        Enabled=false
      '';
  };
}
