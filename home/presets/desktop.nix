{ config, lib, pkgs, specialArgs, ... }:
let
  inherit (config.presets) desktop archlinux;
in
{
  options.presets.desktop = lib.mkEnableOption "Enable desktop preset";

  config = lib.mkIf desktop {
    modules = {
      agenix.enable = true;
      autorandr.enable = true;
      brave.enable = true;
      dunst.enable = true;
      feh.enable = true;
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
      steam.enable = true;
      telegram.enable = true;
      themes.enable = true;
      udiskie.enable = true;
      xdg.enable = true;
      xorg.enable = true;
    };

    home.packages = lib.mkIf (!archlinux) [
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

    modules.i3wm = {
      startup = [
        "copyq"
        "nm-tray"
        "whatsapp-for-linux"
        (if archlinux then "webcord --start-minimized" else "webcord --start-minimized")
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

    xdg.configFile."kwalletrc".text =
      ''
        [Wallet]
        Enabled=false
      '';
  };
}
