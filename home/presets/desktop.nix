{ config, lib, pkgs, specialArgs, ... }:
let inherit (config.presets) desktop; in {
  options.presets.desktop = lib.mkEnableOption "Enable desktop preset";

  config = lib.mkIf desktop {
    pacman.explicits = [
      "extra/lostfiles"
    ];

    pacman.packages = [
      # ######## #### ## #
      # DESK ENV
      # ######## #### ## #

      # tooling
      "chaotic-aur/joycond-git" # joycon drivers
      "chaotic-aur/python-telegram-send"
      "extra/cabextract"
      "extra/p7zip"
      "extra/unzip"
      "extra/yt-dlp"
      "extra/gamemode"
      "extra/cronie" # cron

      # desktop env
      "extra/plasma-meta"
      "aur/konsave"

      # desktop env apps
      "extra/ark" # compressed archiving tool
      "extra/dolphin-plugins" # git
      "extra/dolphin" # file manager
      "extra/ffmpegthumbs" # video thumbs
      "extra/filelight" # disk usage by folder
      "extra/flameshot" # secondary screenshooter
      "extra/spectacle" # screenshooter
      "extra/kcron" # gui cronjob editor
      "extra/kdeconnect" # phone notifications
      "extra/yakuake" # terminal
      "chaotic-aur/64gram-desktop" # telegram, unless it breaks again

      # desktop apps
      "extra/cameractrls"
    ];

    home.packages = [
      (pkgs.writeHostScriptBin "kde-save"
        ''
          #!/usr/bin/bash
          user_at_hostname="''${USER}@$(cat /etc/hostname)"
          konsave -r "$user_at_hostname"
          konsave -s "$user_at_hostname"
          rm "$XDG_CONFIG_HOME/home-manager/assets/konsave/$user_at_hostname.knsv"
          konsave -e "$user_at_hostname" -d "$XDG_CONFIG_HOME/home-manager/assets/konsave"
        '')
      (pkgs.writeHostScriptBin "kde-load"
        ''
          #!/usr/bin/bash
          user_at_hostname="''${USER}@$(cat /etc/hostname)"
          konsave -r "$user_at_hostname"
          konsave -i "$XDG_CONFIG_HOME/home-manager/assets/konsave/$user_at_hostname.knsv"
          konsave -a "$user_at_hostname"
        '')
      (pkgs.writeHostScriptBin "yakuake-newtab"
        ''
          #!/usr/bin/bash

          qdbus6 org.kde.yakuake /yakuake/sessions org.kde.yakuake.addSession

          if [ "$(qdbus6 org.kde.yakuake /yakuake/MainWindow_1 org.qtproject.Qt.QWidget.visible)" = "false" ]; then
            qdbus6 org.kde.yakuake /yakuake/window org.kde.yakuake.toggleWindowState
          else
            if [ "$(qdbus6 org.kde.yakuake /yakuake/MainWindow_1 org.qtproject.Qt.QWidget.isActiveWindow)" = "false" ]; then
              qdbus6 org.kde.yakuake /yakuake/window org.kde.yakuake.toggleWindowState
              qdbus6 org.kde.yakuake /yakuake/window org.kde.yakuake.toggleWindowState
            fi
          fi
        '')
      (pkgs.writeHostScriptBin "yakuake-toggle"
        ''
          #!/usr/bin/bash

          if [ "$(qdbus6 org.kde.yakuake /yakuake/MainWindow_1 org.qtproject.Qt.QWidget.visible)" = "false" ]; then
            # focus if hidden
            qdbus6 org.kde.yakuake /yakuake/window org.kde.yakuake.toggleWindowState
          else
            if [ "$(qdbus6 org.kde.yakuake /yakuake/MainWindow_1 org.qtproject.Qt.QWidget.isActiveWindow)" = "false" ]; then
              # focus if not visible
              qdbus6 org.kde.yakuake /yakuake/window org.kde.yakuake.toggleWindowState
              qdbus6 org.kde.yakuake /yakuake/window org.kde.yakuake.toggleWindowState
            else
              # hide if focused
              qdbus6 org.kde.yakuake /yakuake/window org.kde.yakuake.toggleWindowState
            fi
          fi
        '')
    ];

    modules = {
      audio-share.enable = true;
      electron.enable = true;
      fonts.enable = true;
      jamesdsp.enable = true;
      foot.enable = true;
      loopback-toggle.enable = true;
      micro = { enable = true; desktop = true; };
      rclone.enable = true;
      systemd.imported-variables = [ "PATH" ];
      udiskie.enable = true;
      xdg.enable = true;
    };

    xdg.configFile."kwalletrc".text =
      ''
        [Wallet]
        Enabled=false
      '';
  };
}
