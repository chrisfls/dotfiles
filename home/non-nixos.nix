{ config, pkgs, lib, ... }:
with lib;
let
  cfg = config.module.non-nixos;
in
{
  options.module.non-nixos = {
    enable = mkEnableOption "non-nixos module";
  };
  
  config = mkIf cfg.enable {
    home.packages = with pkgs; [ 
      agenix.packages."${system}".default
      cachix
    ];
  };
}