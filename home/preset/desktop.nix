{ config, lib, pkgs, ... }:
let
  lxqt = pkgs.lxqt;
  cfg = config.extra;
in
{
  imports = [
    ../desktop/wm.nix
    ../desktop/theme.nix
    ../desktop/shell.nix
  ];


  config = lib.mkMerge [
    {
      home.packages = [
        pkgs.alsa-utils
        pkgs.pamixer

        # management
        pkgs.arandr

        # browser
        pkgs.brave
      ];

      extra.qt-theme.enable = true;
      extra.gtk-theme.enable = true;
      extra.icon-theme.enable = true;
      extra.cursor-theme.enable = true;
      extra.font.enable = true;
      extra.shell.xdg-desktop-portal.enable = true;
      extra.shell.notifications.enable = true;
      extra.shell.polkit-agent.enable = true;
      extra.shell.gui-sudo.enable = true;
      extra.shell.ssh-askpass.enable = true;
      extra.shell.file-manager.enable = true;
      extra.shell.volume-mixer.enable = true;
      extra.shell.system-monitor.enable = true;
      extra.shell.clipboard-manager.enable = true;
      extra.shell.screenshot.enable = true;
      extra.shell.screenshot-alt.enable = true;

      programs.rofi = {
        enable = true;
      };

      programs.wezterm = {
        # maybe migrate to foot after https://codeberg.org/dnkl/foot/issues/57
        # unless no issues with gpu accel are found
        enable = true;
      };

      fonts.fontconfig.enable = true;

      services.xsettingsd.enable = true;
    }

    (lib.mkIf config.targets.genericLinux.enable {
      extra.nixGL = {
        enable = true;
        overlay = {
          brave = [ "brave" ];
        };
      };

      extra.nixVulkan.enable = true;
    })
  ];
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
