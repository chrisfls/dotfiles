{ config, lib, pkgs, ... }:
let inherit (config.modules.autorandr) enable; in {
  options.modules.autorandr.enable = lib.mkEnableOption "Enable autorandr module";

  config = lib.mkIf enable {
    # TODO: pacman

    programs.autorandr.enable = true;
    services.autorandr.enable = true;
    xsession.initExtra = "${pkgs.autorandr}/bin/autorandr -c";
  };
}
