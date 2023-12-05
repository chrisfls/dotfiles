{ config, pkgs, lib, ... }:
with lib;
let
  cfg = config.module.paack;
in
{
  options.module.paack = {
    enable = mkEnableOption "paack module";
  };

  config = mkIf cfg.enable {
    home.file = {
      "paack/.envrc".source = ./.envrc;
    };
  };
}
