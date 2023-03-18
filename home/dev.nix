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
    ./fish
    ./daily-use.nix
  ];

  config = mkIf cfg.enable {
    module.fish.enable = true;
    module.daily-use.enable = true;
    
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

    programs.zellij = {
      enable = true;
      settings = {
        theme = "custom";
        themes.custom.fg = "#F2E5BC";
        themes.custom.bg = "#1D2021";
        themes.custom.black = "#928374";
        themes.custom.blue = "#458588";
        themes.custom.cyan = "#689D6A";
        themes.custom.green = "#B8BB26";
        themes.custom.magenta = "#B16286";
        themes.custom.orange = "#D25F1F";
        themes.custom.red = "#CC241D";
        themes.custom.white = "#A89984";
        themes.custom.yellow = "#D79921";
      };
    };

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