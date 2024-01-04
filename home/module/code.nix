{ config, lib, pkgs, ... }:
let inherit (config.module.code) enable; in {
  options.module.code.enable = lib.mkEnableOption "Enable code module";

  config = lib.mkIf enable {
    home.packages = [ pkgs.vscode ];
    # pacman.usr.vscode = [ "chaotic-aur/visual-studio-code-bin" ];
  };
}
