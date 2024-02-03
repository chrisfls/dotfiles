{ config, lib, pkgs, specialArgs, ... }:
let
  inherit (config.presets) archlinux;
  inherit (config.modules.code) enable;
in
{
  options.modules.code.enable = lib.mkEnableOption "Enable code module";

  config = lib.mkIf enable {
    home.packages = lib.mkIf (!archlinux) [ pkgs.vscode ];
    pacman.packages = [ "chaotic-aur/visual-studio-code-bin" ];
  };
}
