{ config, lib, pkgs, ... }:
let inherit (config.modules.keychain) enable extraConfig; in {
  options.modules.keychain.enable = lib.mkEnableOption "Enable keychain module";

  config = lib.mkIf enable {
    pacman.packages = [ "extra/keychain" ];

    modules.bash.extraConfig =
      ''
        eval "$(SHELL=bash keychain --eval --quiet id_ed25519)"
      '';

    modules.fish.extraConfig =
      ''
        SHELL=fish keychain --eval --quiet id_ed25519 | source
      '';
  };
}
