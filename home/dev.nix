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
    ./fish
    ./zellij.nix
  ];

  config = mkIf cfg.enable {
    module.bash.enable = true;
    module.zellij.enable = true;
    module.fish.enable = true;
    
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
        # "s" = "git --git-dir=$HOME/.system.git --work-tree=/etc/nixos";
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