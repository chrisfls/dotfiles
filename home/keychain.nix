{ config, pkgs, lib, ... }:
with lib;
let
  cfg = config.module.keychain;
in
{
  options.module.keychain = {
    enable = mkEnableOption "keychain module";
  };

  config = mkIf cfg.enable {
    programs.keychain = {
      enable = true;
      keys = [ "id_ed25519" ];
    };
  };
}
