{ config, lib, pkgs, ... }:
let inherit (config.module.sublime) enable; in {
  options.module.sublime.enable = lib.mkEnableOption "Enable sublime module";
  config = lib.mkIf enable {
    home.packages = [
      pkgs.sublime4
      pkgs.sublime-merge-dev
    ];

    pacman.overrides.sublime4 = [ "chaotic-aur/sublime-text-4" ];

    nixpkgs.config.permittedInsecurePackages = [ "openssl-1.1.1w" ];
  };
}
