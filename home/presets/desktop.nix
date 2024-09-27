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

      # desktop env
      "extra/plasma-meta"
      "aur/konsave"
      # "extra/bitwarden-cli"

      # desktop env apps
      # REVIEW: migrate apps to flatpak
      # "extra/ark" # compressed archiving tool
      "extra/dolphin-plugins" # git
      "extra/dolphin" # file manager
      # "extra/featherpad" # text editor, replaced with kwrite
      "extra/ffmpegthumbs" # video thumbs
      # "extra/filelight" # disk usage by folder
      # "extra/gwenview" # image viewer
      # "extra/juk" # lightweight music player
      # "extra/kalk" # calculator
      # "extra/kamoso" # webcam recorder
      # "extra/kcharselect" # character selector
      "extra/kcron" # gui cronjob editor
      "extra/cronie" # cron implementation
      "extra/kdeconnect" # phone notifications
      # "extra/kdenlive" # video editor
      # "extra/kolourpaint" # image editor
      # "extra/partitionmanager" # partition manager REVIEW: remove
      # REVIEW: replaced spectacle with flameshot because of broken libraries
      # REVIEW: not using flameshot flatpak because of broken clipboard
      "extra/flameshot" # screenshooter 

      # desktop apps
      # "extra/qbittorrent" # torrent downloader
      # "extra/cameractrls"
      # "extra/inkscape"

      # ######## #### ## #
      # MY PKGS
      # ######## #### ## #

      # "chaotic-aur/anydesk-bin" # remote access
      # "chaotic-aur/logseq-desktop-bin" # work logs
      # "chaotic-aur/moonlight-qt" # game stream
      # "chaotic-aur/parsec-bin" # game stream (poor performance on linux)
      # "extra/vesktop-git" # audio chat with friends (replaced with flatpak webcord)
      # "extra/obsidian" # personal knowledge manager
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
