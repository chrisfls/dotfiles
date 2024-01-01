{ config, lib, pkgs, ... }:
let
  cfg = config.module.code;
in
{
  options.module.code.enable = lib.mkEnableOption "Enable code module";

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.vscode ];
  };
}
