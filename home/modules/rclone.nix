{ config, lib, pkgs, ... }:
let inherit (config.modules.rclone) enable; in {
  options.modules.rclone.enable = lib.mkEnableOption "Enable rclone module";

  config = lib.mkIf enable {
    pacman.packages = [ "extra/rclone" ];

    xdg.configFile."systemd/user/rclone@.service".text =
      ''
        # Credits: kabili207 - https://gist.github.com/kabili207/2cd2d637e5c7617411a666d8d7e97101

        [Unit]
        Description=rclone: Remote FUSE filesystem for cloud storage config %I
        Documentation=man:rclone(1)
        After=network-online.target
        Wants=network-online.target
        AssertPathIsDirectory="%h/mnt/%I"

        [Service]
        Type=notify
        ExecStart=/usr/bin/rclone mount "%I:" "%h/Desktop/rclone/%I"
        ExecStop=fusermount -u "%h/Desktop/rclone/%I"

        [Install]
        WantedBy=default.target
      '';
  };
}
