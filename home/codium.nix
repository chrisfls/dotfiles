{ pkgs, ... }:
let
  ext = pkgs.vscode-extensions;
in
{
  programs.vscode = {
    enable = true;
    package = pkgs.vscodium;
    extensions = [
      ext.editorconfig.editorconfig
      ext.waderyan.gitblame
      ext.bbenoist.nix
      ext.b4dm4n.vscode-nixpkgs-fmt
    ] ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [
      {
        name = "yuck";
        publisher = "eww-yuck";
        version = "0.0.3";
        sha256 = "sha256-DITgLedaO0Ifrttu+ZXkiaVA7Ua5RXc4jXQHPYLqrcM=";
      }
      {

        name = "default-keys-windows";
        publisher = "smcpeak";
        version = "0.0.10";
        sha256 = "sha256-v1JY5ZGWOfF14H235Y9CLlPwIvmNwCeRhIkdmcgCCFU=";
      }
    ];
  };

  extra.nixGL.overlay = { vscodium = [ ]; };
}
