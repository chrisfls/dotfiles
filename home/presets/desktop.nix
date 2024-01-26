{ config, lib, pkgs, specialArgs, ... }:
let
  inherit (config.presets) desktop non-nixos;
in
{
  options.presets.desktop = lib.mkEnableOption "Enable desktop preset";

  config = lib.mkIf desktop {
    modules = {
      agenix.enable = true;
      autorandr.enable = true;
      brave.enable = true;
      dunst.enable = true;
      fontconfig.enable = true;
      fonts.enable = true;
      i3wm.enable = true;
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
      telegram.enable = true;
      themes.enable = true;
      xdg.enable = true;
      xorg.enable = true;
    };

    home.packages = lib.mkIf (!non-nixos) [
      # ######## #### ## #
      # DESK ENV
      # ######## #### ## #

      # cli
      pkgs.alsa-utils
      pkgs.pamixer

      # dialogs
      pkgs.libsForQt5.kdialog # dialogs and widgets
      pkgs.lxqt.lxqt-openssh-askpass # ssh prompter
      pkgs.lxqt.lxqt-policykit # policykit prompter
      pkgs.lxqt.lxqt-sudo # gui-sudo prompter

      # misc
      pkgs.nm-tray # network manager

      # core apps
      pkgs.lxqt.lxqt-archiver # archiver
      pkgs.lxqt.pavucontrol-qt # sound mixer
      pkgs.lxqt.pcmanfm-qt # file manager

      # common apps
      pkgs.copyq # clipboard manager
      pkgs.featherpad # simple text editor
      pkgs.libsForQt5.juk # music player [or elisa]
      pkgs.libsForQt5.kolourpaint # simple image editor
      pkgs.lxqt.qps # system monitor
      pkgs.mpc-qt # video player [or haruna/QMPlay2/kmplayer/dragonplayer/mpv]
      pkgs.qalculate-qt # calculator

      # ######## #### ## #
      # MY PKGS
      # ######## #### ## #

      pkgs.anydesk
      pkgs.logseq
      pkgs.moonlight-qt
      pkgs.obsidian
      pkgs.parsec-bin
      pkgs.qbittorrent
      pkgs.webcord-vencord
      pkgs.whatsapp-for-linux
      pkgs.wootility
      # pkgs.soulseekqt
    ];

    pacman.packages = [
      # ######## #### ## #
      # DESK ENV
      # ######## #### ## #

      # cli
      "extra/alsa-utils"
      "extra/pamixer"
      "extra/udisks2"
      "extra/udiskie"

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

      # ######## #### ## #
      # MY PKGS
      # ######## #### ## #

      "chaotic-aur/anydesk-bin"
      "chaotic-aur/logseq-desktop-bin"
      "chaotic-aur/moonlight-qt"
      "extra/obsidian"
      "chaotic-aur/parsec-bin"
      "extra/qbittorrent"
      "chaotic-aur/vesktop"
      "chaotic-aur/whatsapp-for-linux"
    ];

    xdg.mimeApps.enable = true;

    # TODO: move udiskie to own module
    xdg.configFile."udiskie/config.yml".text =
      ''
        program_options:
          automount: true
          notify: true
          tray: auto
      '';

    systemd.user.services.udiskie = {
      Unit = {
        Description = "udiskie mount daemon";
        Requires = "tray.target";
        After = [ "graphical-session-pre.target" "tray.target" ];
        PartOf = [ "graphical-session.target" ];
      };

      Service.ExecStart =
        if non-nixos then "/usr/bin/udiskie"
        else "${pkgs.udiskie}/bin/udiskie";

      Install.WantedBy = [ "graphical-session.target" ];
    };
  };
}
