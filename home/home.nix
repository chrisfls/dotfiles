{ config, pkgs, lib, ... }:
with lib;
let
  cfg = config.module.home;
in
{
  options.module.home = {
    enable = mkEnableOption "home module";
    username = mkOption { type = types.str; };
  };

  config = mkIf cfg.enable {
    home = {
      inherit (cfg) username; homeDirectory = "/home/${cfg.username}";
    };
    systemd.user.startServices = "sd-switch";
  };
}
