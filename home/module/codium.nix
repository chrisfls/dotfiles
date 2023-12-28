{ config, lib, pkgs, ... }:
let
  cfg = config.module.codium;
  ext = pkgs.vscode-extensions;
in
{
  options.module.codium.enable = lib.mkEnableOption "Enable codium module";

  config = lib.mkIf cfg.enable {
    programs.vscode = {
      enable = true;
      package = pkgs.vscodium;
      extensions = pkgs.vscode-utils.extensionsFromVscodeMarketplace
        (import ./codium/extensions.nix).extensions;
    };
  };
}
