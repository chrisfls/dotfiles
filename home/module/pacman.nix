{ config, lib, pkgs, specialArgs, ... }:
let
  inherit (config.pacman) enable packages;

  inherit (builtins) concatStringsSep filter isAttrs isList;
  inherit (lib) mkEnableOption mkIf mkOption types;
  inherit (lib.attrsets) attrValues;
  inherit (lib.lists) concatMap;
  inherit (lib.strings) hasPrefix;
  inherit (lib.trivial) pipe;

  quote = str: "'${str}'";
in
{
  options.pacman = {
    enable = mkEnableOption "Enable pacman module";

    packages = mkOption {
      type = types.listOf types.str;
      default = [ ];
    };
  };

  config = mkIf enable {
    home.packages =
      let
        repo-packages = pipe packages [
          (filter (name: !(hasPrefix "aur/" name)))
          (map quote)
        ];

        aur-packages = pipe packages [
          (filter (name: hasPrefix "aur/" name))
          (map quote)
        ];
      in
      [
        (pkgs.writeShellScriptBin "pacman-switch"
          ''
            sudo pacman -Sy --needed --noconfirm archlinux-keyring chaotic-keyring && \
            sudo pacman -Su --needed --noconfirm ${concatStringsSep " " repo-packages} && \
            paru -Sua --needed --noconfirm ${concatStringsSep " " aur-packages}
          '')
      ];
  };
}
