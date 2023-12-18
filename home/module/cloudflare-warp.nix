{ config, inputs, lib, pkgs, ... }:
let
  cfg = config.extra.cloudflare-warp;
in
{
  options.extra.cloudflare-warp.enable = lib.mkEnableOption "Enable cloudflare-warp module";

  config = lib.mkIf cfg.enable {
    xdg = {
      enable = true;
      dataFile = {
        "warp/accepted-teams-tos.txt".text = "yes\n";
        "warp/accepted-tos.txt".text = "yes\n";
      };
    };
  };
}
