{ config, lib, pkgs, specialArgs, ... }:
let
  inherit (config.presets) non-nixos;
  inherit (config.modules.code) enable;
in
{
  options.modules.code.enable = lib.mkEnableOption "Enable code module";

  config = lib.mkIf enable {
    home.packages =
      if non-nixos then [ ]
      else [ pkgs.vscode ];

    pacman.packages = [ "chaotic-aur/visual-studio-code-bin" ];
  };
}
