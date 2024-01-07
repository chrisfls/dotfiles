{ config, lib, pkgs, specialArgs, ... }:
let
  inherit (config.module.code) enable;

  mesa = specialArgs.mesa.wrapIf config.preset.non-nixos;
in
{
  options.module.code.enable = lib.mkEnableOption "Enable code module";

  config = lib.mkIf enable {
    home.packages = [
      (mesa { pkg = pkgs.vscode; })
    ];
  };
}
