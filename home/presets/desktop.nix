{ config, lib, pkgs, specialArgs, ... }:
let inherit (config.presets) desktop; in {
  options.presets.desktop = lib.mkEnableOption "Enable desktop preset";

  config = lib.mkIf desktop {
    modules = {
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
      rclone.enable = true;
      polybar.enable = true;
      qview.enable = true;
      rofi.enable = true;
      screenshot.enable = true;
      telegram.enable = true;
      themes.enable = true;
      xdg.enable = true;
      xorg.enable = true;
    };

    home.packages = [
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

      # personal cli pkgs
      pkgs.wootility

      # personal apps
      pkgs.qbittorrent
      pkgs.anydesk
      pkgs.gimp
      pkgs.moonlight-qt
      pkgs.parsec-bin
      pkgs.webcord-vencord
      pkgs.whatsapp-for-linux
      pkgs.logseq
      pkgs.ferdium
      # pkgs.soulseekqt

      # needed until these are done:
      #  - https://github.com/NixOS/nixpkgs/issues/228179
      #  - https://github.com/NixOS/nixpkgs/pull/273263 
      (pkgs.libsForQt5.audiotube.overrideAttrs (old: {
        buildInputs = old.buildInputs ++ [ pkgs.libsForQt5.kpurpose ];
      }))
    ];


    services.udiskie.enable = true;
  };
}
