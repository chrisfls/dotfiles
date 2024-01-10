{ config, lib, pkgs, specialArgs, ... }:
let inherit (config.module.code) enable; in
{
  options.module.code.enable = lib.mkEnableOption "Enable code module";

  config = lib.mkIf enable {
    home.packages = [ pkgs.vscode ];
  };
}
