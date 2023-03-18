{ config, pkgs, lib, ... }:
with lib;
let
  cfg = config.module.kakoune;
in
{
  options.module.kakoune = {
    enable = mkEnableOption "kakoune module";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      kak-lsp
      xplr
    ];
    programs.kakoune = {
      enable = true;
    };
  };
}
