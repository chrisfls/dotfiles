{ config, lib, pkgs, ... }:
let inherit (config.modules.rclone) enable; in {
  options.modules.rclone.enable = lib.mkEnableOption "Enable rclone module";

  config = lib.mkIf enable {
    pacman.packages = [ "extra/rclone" ];

    home.packages = [
      (pkgs.writeHostScriptBin "rclone-sync"
        ''
          hosname=$(cat /etc/hostname)
          mode="--resilient"

          if [ "$1" == "setup" ] || [ "$1" == "resync" ]; then
            mode="--resync"
          fi

          _setup() {
            eval local="$2"
            eval backup="$2.bak"

            echo "[SETUP] Initializing setup for '$local'"

            if [ -d "$local" ]; then
              if [ -d "$backup" ]; then
                echo "[SETUP] ERROR: backup for '$local' already exists at '$backup'"
                exit 1
              fi

              echo "[SETUP] Backing up '$local' content at '$backup'"
              mv "$local" "$backup"
            fi

            echo "[SETUP] Copying 'cflsousa:Sync/$1' to '$local'"
            rclone copy "cflsousa:Sync/$1" "$local" -P
            echo "[SETUP] Finished setup for '$local'"
          }

          _sync() {
            eval local="$2"
            echo "[SYNC] Syncing '$local' with 'cflsousa:Sync/$1'"
            rclone bisync "$local" "cflsousa:Sync/$1" $mode --create-empty-src-dirs --force -P
            echo "[SYNC] Finished sync for '$local'"
          }

          _perform() {
            case "$hosname" in
              "arch-rmxp")
                retroarch="~/.config/retroarch"
                $1 "RetroArch/arch-rmxp/config" "$retroarch/config" &
                $1 "RetroArch/all/recordings" "$retroarch/records" &
                $1 "RetroArch/all/saves" "$retroarch/saves" &
                $1 "RetroArch/all/screenshots" "$retroarch/screenshots" &
                $1 "RetroArch/all/states" "$retroarch/states" &
                $1 "RetroArch/all/system" "$retroarch/system" &
                ;;
            esac
            wait
          }

          case "$1" in
            "setup")
              _perform _setup
              mode="--resync"
              _perform _sync
              ;;
            "resync")
              _perform _sync
              ;;
            "sync")
              _perform _sync
              ;;
            *)
              echo "Invalid option. Usage: $0 {setup|resync|sync}"
              exit 1
              ;;
          esac
        '')
    ];


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
        ExecStart=/usr/bin/rclone mount "%I:" "%h/Desktop/cloud/%I"
        ExecStop=fusermount -u "%h/Desktop/cloud/%I"

        [Install]
        WantedBy=default.target
      '';
  };
}
