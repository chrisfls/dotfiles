{ config, lib, pkgs, ... }:
let inherit (config.module.autorandr) enable; in {
  options.module.autorandr.enable = lib.mkEnableOption "Enable autorandr module";

  config = lib.mkIf enable {
    services.autorandr.enable = true;
    programs.autorandr.enable = true;
    xsession.initExtra = "${pkgs.autorandr}/bin/autorandr -c";
  };
}
