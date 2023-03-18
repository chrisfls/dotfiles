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
        themes.custom.fg = "#EBDBB2";
        themes.custom.bg = "#000000";
        themes.custom.black = "#928374";
        themes.custom.blue = "#99C6CA";
        themes.custom.cyan = "#7EC16E";
        themes.custom.green = "#B8BB26";
        themes.custom.magenta = "#D3869B";
        themes.custom.orange = "#F77537";
        themes.custom.red = "#F42C3E";
        themes.custom.white = "#EBDBB2";
        themes.custom.yellow = "#FABD2F";
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