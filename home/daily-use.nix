{ config, pkgs, lib, specialArgs, ... }:
with lib;
with specialArgs;
let
  cfg = config.module.daily-use;
in
{
  options.module.daily-use = {
    enable = mkEnableOption "daily use module";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      # tooling
      htop
      traceroute
      killall
      neofetch
      mosh # probably never used
      nix-index
    ];
  };
}
