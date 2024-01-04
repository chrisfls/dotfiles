{ config, lib, pkgs, ... }:
let inherit (config.module.code) enable; in {
  options.module.code.enable = lib.mkEnableOption "Enable code module";

  config = lib.mkIf enable {
    home.packages = [ pkgs.usr.vscode ];
    pacman.overrides.vscode = [ "chaotic-aur/visual-studio-code-bin" ];
  };
}
