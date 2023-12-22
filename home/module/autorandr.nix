{ config, lib, pkgs, ... }:
let
  cfg = config.extra.autorandr;
in
{
  options.extra.autorandr.enable = lib.mkEnableOption "Enable autorandr module";

  config = lib.mkIf cfg.enable {
    services.autorandr.enable = true;
    xsession.initExtra = "${pkgs.autorandr}/bin/autorandr -c";
  };
}
