{ config, lib, pkgs, ... }:
let inherit (config.modules.any-nix-shell) enable extraConfig; in {
  options.modules.any-nix-shell.enable = lib.mkEnableOption "Enable any-nix-shell module";

  config = lib.mkIf enable {
    home.packages = [ pkgs.any-nix-shell ];

    modules.fish.extraConfig =
      ''
        any-nix-shell fish --info-right | source
      '';
  };
}
