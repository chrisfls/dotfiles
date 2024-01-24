{ config, lib, pkgs, specialArgs, ... }:
let
  inherit (config.presets) non-nixos;
  inherit (config.modules.code) enable;
in
{
  options.modules.code.enable = lib.mkEnableOption "Enable code module";

  config = lib.mkIf enable {
    home.packages = lib.mkIf (!non-nixos) [ pkgs.vscode ];
    pacman.packages = [ "chaotic-aur/visual-studio-code-bin" ];
  };
}
