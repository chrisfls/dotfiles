{ config, pkgs, lib, ... }:
with lib;
let
  cfg = config.module.micro;
in
{
  options.module.micro = {
    enable = mkEnableOption "micro module";
  };

  config = mkIf cfg.enable {
    programs.micro = {
      enable = true;
      settings = {
        colorscheme = "solarized-tc";
      };
    };
  };
}
