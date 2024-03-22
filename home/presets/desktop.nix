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

      # tooling
      "chaotic-aur/joycond-git" # joycon drivers

      # desktop env
      "extra/plasma-meta"
      "aur/savedesktop" # TODO: decide between this and konsave (depends on gtk)
      "aur/konsave" # TODO: decide between this and savedesktop

      # desktop env apps
      "extra/ark" # compressed archiving tool
      "extra/dolphin-plugins" # git
      "extra/dolphin" # file manager
      "extra/featherpad" # text editor
      "extra/ffmpegthumbs" # video thumbs
      "extra/filelight" # disk usage by folder
      "extra/gwenview" # image viewer
      "extra/juk" # lightweight music player
      "extra/kalk" # calculator
      "extra/kamoso" # webcam recorder
      "extra/kcharselect" # character selector
      "extra/kdeconnect" # phone notifications
      "extra/kdenlive" # video editor
      "extra/ktorrent" # torrent downloader
      "extra/kolourpaint" # image editor
      "extra/qbittorrent" # torrents
      "extra/partitionmanager" # partition manager TODO: remove?
      "extra/spectacle" # screenshooter

      # desktop apps
      "chaotic-aur/bottles" # TODO: replace with qt/kde equivalent?
      "extra/cameractrls"
      "extra/inkscape"

      # ######## #### ## #
      # MY PKGS
      # ######## #### ## #

      "chaotic-aur/anydesk-bin" # remote access
      "chaotic-aur/logseq-desktop-bin" # work logs
      "chaotic-aur/moonlight-qt" # game stream
      "chaotic-aur/parsec-bin" # game stream (poor performance on linux)
      "chaotic-aur/webcord" # audio chat with friends
      "extra/obsidian" # personal knowledge manager
    ];

    modules = {
      brave.enable = true;
      electron.enable = true;
      fonts.enable = true;
      jamesdsp.enable = true;
      foot.enable = true;
      loopback-toggle.enable = true;
      micro = { enable = true; desktop = true; };
      rclone.enable = true;
      steam.enable = true;
      systemd.imported-variables = [ "PATH" ];
      telegram.enable = true;
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
