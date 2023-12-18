{ config, lib, pkgs, ... }:
let
  lxqt = pkgs.lxqt;
  nixgl = config.targets.genericLinux.enable;
in
{
  imports = [ ../module ];

  extra.bar.enable = true;
  extra.browser.enable = true;
  extra.fontconfig.enable = true;
  extra.fonts.enable = true;
  extra.hotkeys.enable = true;
  extra.menu.enable = true;
  extra.notifications.enable = true;
  extra.screenshot.enable = true;
  extra.terminal.enable = true;
  extra.themes.enable = true;
  extra.window-manager.enable = true;

  home.packages = [
    # cli apps
    pkgs.alsa-utils
    pkgs.pamixer
    pkgs.xclip

    # desktop components
    lxqt.xdg-desktop-portal-lxqt # qt integration with xdg-desktop-portal
    lxqt.lxqt-policykit # policykit prompter
    lxqt.lxqt-sudo # gui-sudo prompter
    lxqt.lxqt-openssh-askpass # ssh prompter

    # desktop environment apps
    pkgs.arandr # manual display config
    lxqt.lxqt-config # config editor
    lxqt.pavucontrol-qt # sound mixer
    lxqt.pcmanfm-qt # file manager

    # desktop apps
    lxqt.qlipper # clipboard manager [alt: pkgs.copyq; but seems bloated]
    lxqt.qps # system monitor
  ];

  #
  # enable hardware acceleration
  #

  extra.nixGL = {
    enable = nixgl;
    overlay.lxqt = {
      # desktop components
      lxqt-policykit = [ "lxqt-policykit-agent" ];
      lxqt-sudo = [ "lxqt-sudo" ];
      lxqt-openssh-askpass = [ "lxqt-openssh-askpass" ];

      # desktop environment apps
      lxqt-config = [ "lxqt-config" ];
      pavucontrol-qt = [ "pavucontrol-qt" ];
      pcmanfm-qt = [ "pcmanfm-qt" ];
      qlipper = [ "qlipper" ];

      # desktop apps
      qps = [ "qps" ];
    };
  };

  extra.nixVulkan.enable = nixgl;

  #
  # startx support
  #

  xsession.enable = true;

  home.file.".xinitrc" = {
    executable = true;
    text = ''
      #!/bin/sh
      # last if block from /etc/X11/xinit/xinitrc
      if [ -d /etc/X11/xinit/xinitrc.d ] ; then
      for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
        [ -x "$f" ] && . "$f"
      done
      unset f
      fi

      [[ -f ~/.xsession ]] && . ~/.xsession
    '';
  };

  #
  # extra config files
  #

  xdg = {
    enable = true;
    userDirs.enable = true;
    configFile = {
      "xdg-desktop-portal/portals.conf".text =
        ''
          [preferred]
          default=gtk
          org.freedesktop.impl.portal.FileChooser=lxqt
        '';
      "systemd/user/xdg-desktop-portal.service.d/override.conf".text =
        ''
          [Service]
          Environment="XDG_CURRENT_DESKTOP=KDE"
        '';
    };
  };

  fonts.fontconfig.enable = true;

  services.xsettingsd.enable = true;
}

/*
  if [ -z "$HM_XPROFILE_SOURCED" ]; then
  . "/home/kress/.xprofile"
  fi
  unset HM_XPROFILE_SOURCED

  systemctl --user start hm-graphical-session.target

  /nix/store/8yr7g35af354a0160fxfmnj0nqq8hl3s-xrdb-1.2.2/bin/xrdb -merge /home/kress/.Xresources
  eval "$(SHELL=bash /nix/store/0v63rbc7p7k22cnqg42qb1nzi8y4brip-keychain-2.8.5/bin/keychain --eval --quiet id_ed25519)"

  /nix/store/7wq4hyld7z3wffh9m3j9v4wvz1kr81q5-xsetroot-1.1.3/bin/xsetroot -xcf /nix/store/0km6448r6yggd6avznq5158rjkgafm56-breeze-qt5-5.27.9-bin/share/icons/'breeze_cursors'/cursors/'breeze_cursors' 24


  /nix/store/ps52bpf2yw2lsqdn3w5zy41v1dzb1vgv-i3-4.23/bin/i3

  systemctl --user stop graphical-session.target
  systemctl --user stop graphical-session-pre.target

  # Wait until the units actually stop.
  while [ -n "$(systemctl --user --no-legend --state=deactivating list-units)" ]; do
  sleep 0.5
  done

  systemctl --user unset-environment 'DBUS_SESSION_BUS_ADDRESS' 'DISPLAY' 'SSH_AUTH_SOCK' 'XAUTHORITY' 'XDG_DATA_DIRS' 'XDG_RUNTIME_DIR' 'XDG_SESSION_ID' 'QT_PLUGIN_PATH' 'QML2_IMPORT_PATH' 'QT_QPA_PLATFORMTHEME' 'QT_STYLE_OVERRIDE'
*/
