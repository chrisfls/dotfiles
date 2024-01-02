{ config, lib, pkgs, ... }:
let inherit (config.module.code) enable; in {
  options.module.code.enable = lib.mkEnableOption "Enable code module";

  config.home.packages = lib.mkIf enable [ pkgs.vscode ];
}
