{ config, lib, pkgs, ... }:
let inherit (config.module.autorandr) enable; in {
  options.module.autorandr.enable = lib.mkEnableOption "Enable autorandr module";

  config = lib.mkIf enable {
    pacman.overrides.autorandr = [ "extra/autorandr" ];
    programs.autorandr.enable = true;
    services.autorandr.enable = true;
    xsession.initExtra = "${pkgs.usr.autorandr}/bin/autorandr -c";
  };
}
