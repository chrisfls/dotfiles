{ config, lib, pkgs, ... }:
let
  cfg = config.module.autorandr;
in
{
  options.module.autorandr.enable = lib.mkEnableOption "Enable autorandr module";

  config = lib.mkIf cfg.enable {
    services.autorandr.enable = true;
    programs.autorandr.enable = true;
    xsession.initExtra = "${pkgs.autorandr}/bin/autorandr -c";
  };
}
