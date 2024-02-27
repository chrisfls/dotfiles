hosname=$(cat /etc/hostname)

mode="$([[ "$1" == "setup" || "$1" == "resync" ]] && echo "--resync" || echo "--resilient")"

_copy_down() {
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
  rclone copyto "cflsousa:Sync/$1" "$local" -P
  echo "[SETUP] Finished setup for '$local'"
}


_sync_up() {
  eval local="$2"
  echo "[SYNC] Copying '$local' to 'cflsousa:Sync/$1'"
  rclone copyto "$local" "cflsousa:Sync/$1" -P
}

_sync() {
  case "$1" in
    "up")
      _sync_up "${@:2}"
      ;;
    "down")
      _copy_down "${@:2}"
      ;;
  esac
}

_bisync_up() {
  eval local="$2"
  echo "[BISYNC] Syncing '$local' with 'cflsousa:Sync/$1'"
  rclone bisync "$local" "cflsousa:Sync/$1" $mode --create-empty-src-dirs --force -P "${@:3}"
  echo "[BISYNC] Finished sync for '$local'"
}

_bisync() {
  case "$1" in
    "up")
      _bisync_up "${@:2}"
      ;;
    "down")
      _copy_down "${@:2}"
      ;;
  esac
}

_perform() {
  case "$hosname" in
    "arch-rmxp")
      retroarch="~/.config/retroarch"
      _bisync $1 "RetroArch/all/recordings" "$retroarch/records" &
      _bisync $1 "RetroArch/all/saves" "$retroarch/saves" &
      _bisync $1 "RetroArch/all/screenshots" "$retroarch/screenshots" &
      _bisync $1 "RetroArch/all/states" "$retroarch/states" &
      _bisync $1 "RetroArch/all/system" "$retroarch/system" &
      _bisync $1 "RetroArch/arch-rmxp/config" "$retroarch/config" &
      _sync $1 "RetroArch/arch-rmxp/retroarch.cfg" "$retroarch/retroarch.cfg" &
      ;;
  esac
  wait
}

case "$1" in
  "setup")
    _perform down
    _perform up
    ;;
  "resync")
    _perform up
    ;;
  "sync")
    _perform up
    ;;
  *)
    echo "Invalid option. Usage: $0 {setup|resync|sync}"
    exit 1
    ;;
esac
