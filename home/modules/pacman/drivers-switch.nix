{ config, lib, pkgs, ... }:
let inherit (config.pacman) enable; in lib.mkIf enable {
  home.packages = [
    (pkgs.writeShellScriptBin "drivers-switch"
      ''
        pkg() {
          pactree -l -u $1
        }

        pkgs=()
        pkgs+=($(pkg intel-media-driver))
        pkgs+=($(pkg libglvnd))
        pkgs+=($(pkg libva))
        pkgs+=($(pkg libx11))
        pkgs+=($(pkg libxau))
        pkgs+=($(pkg libxcb))
        pkgs+=($(pkg libxdmcp))
        pkgs+=($(pkg mesa))
        pkgs+=($(pkg vulkan-intel))
        pkgs+=($(pkg wayland))

        declare -A assocPkgs

        for file in "''${pkgs[@]}"; do
          assocPkgs[$file]=1
        done

        pkgs=("''${!assocPkgs[@]}")

        get() {
          pacman -Ql "$1" | grep '/usr/' | grep -v '/$' | awk -F '/usr/' '{print $2}'
        }

        files=()

        for pkg in "''${pkgs[@]}"; do
          files+=($(get "$pkg"))
        done

        declare -A assocFiles

        for file in "''${files[@]}"; do
          assocFiles[$file]=1
        done

        files=("''${!assocFiles[@]}")

        for file in "''${files[@]}"; do
          mkdir -p $(dirname "${config.xdg.configHome}/drivers/$file")
          ln -s "/usr/$file" "${config.xdg.configHome}/drivers/$file" > /dev/null
        done
      '')
  ];
}
