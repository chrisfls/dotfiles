{ config, lib, pkgs, ... }:
let
  inherit (config.pacman) enable packages;

  quote = str: "'${str}'";

  repo-packages = lib.trivial.pipe packages [
    (builtins.filter (name: !(lib.strings.hasPrefix "aur/" name)))
    (builtins.map quote)
  ];

  aur-packages = lib.trivial.pipe packages [
    (builtins.filter (name: lib.strings.hasPrefix "aur/" name))
    (builtins.map quote)
  ];
in
lib.mkIf enable {
  home.packages = [
    (pkgs.writeShellScriptBin "pacman-switch"
      ''
        sudo pacman -Sy --needed --noconfirm archlinux-keyring chaotic-keyring && \
        sudo pacman -Su --needed --noconfirm ${builtins.concatStringsSep " " repo-packages} && \
        paru -Sua --needed --noconfirm ${builtins.concatStringsSep " " aur-packages}
      '')
  ];
}
