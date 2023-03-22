{ config, pkgs, lib, ... }:
with lib;
let
  cfg = config.module.dev;
in
{
  options.module.dev = {
    enable = mkEnableOption "dev module";
  };
  
  imports = [
    ./bash
    ./emacs
    ./fish
  ];

  config = mkIf cfg.enable {
    module = {
      bash.enable = true;
      emacs.enable = true;
      fish = {
        enable = true;
        autostart = true;
      };
    };
    
    home.packages = with pkgs; [
      # tools
      wget
      jq
      which

      # nix
      nixpkgs-fmt
      any-nix-shell

      # languagetool
      ltex-ls
      adoptopenjdk-jre-openj9-bin-16
    ];

    programs.fish = {
      shellAliases = {
        "e" = "code";
        "da" = "direnv allow";
        "g" = "git";
      };
      functions = {
        shell = ''
          # prefer using direnv as using nix-shell directly is slow
          # also depends on any-nix-shell to maintain current shell
          nix-shell shell.nix $argv[1..-1]
        '';
        develop = ''
          nix develop "/etc/nixos/shells/$argv[1]" -c fish $argv[2..-1];
        '';
      };
    };
  };
}