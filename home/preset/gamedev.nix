{ config, lib, pkgs, specialArgs, ... }:
let
  enable = config.preset.gamedev;
  mesa = specialArgs.mesa.wrapIf config.preset.non-nixos;
in
{
  options.preset.gamedev = lib.mkEnableOption "Enable gamedev preset";

  config = (lib.mkIf enable {
    preset.development = true;

    module.steam.enable = true;

    home.packages = [
      pkgs.aseprite
      pkgs.krita
      pkgs.tiled
      pkgs.lmms
    ];

    nixpkgs.config.allowUnfree = true;
  });
}
