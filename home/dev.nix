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
  ];

  config = mkIf cfg.enable {
    module.fish.enable = true;

    programs.fish = {
      enable = true;
      shellAliases = {
        "e" = "code";
        "da" = "direnv allow";
        "g" = "git";
        # "s" = "git --git-dir=$HOME/.system.git --work-tree=/etc/nixos";
      };
    };
  };
}