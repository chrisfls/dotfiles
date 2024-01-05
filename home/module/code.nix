{ config, lib, pkgs, specialArgs, ... }:
let
  inherit (config.module.code) enable;
  inherit (specialArgs) mesa;
in {
  options.module.code.enable = lib.mkEnableOption "Enable code module";

  config = lib.mkIf enable {
    home.packages = [
      (mesa.wrapIf config.preset.non-nixos
        { package = pkgs.vscode; })
    ];
  };
}
