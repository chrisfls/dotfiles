{ config, pkgs, lib, ... }:
with lib;
let
  cfg = config.module.warp;
in
{
  options.module.warp = {
    enable = mkEnableOption "warp module";
  };

  config = mkIf cfg.enable {
    xdg = {
      enable = true;
      dataFile = {
        "warp/accepted-teams-tos.txt".text = "yes\n";
        "warp/accepted-tos.txt".text = "yes\n";
      };
    };
  };
}
