{ config, inputs, lib, pkgs, ... }:
let inherit (config.module.cloudflare-warp) enable; in {
  options.module.cloudflare-warp.enable = lib.mkEnableOption "Enable cloudflare-warp module";

  config.xdg.dataFile = lib.mkIf enable {
    "warp/accepted-teams-tos.txt".text = "yes\n";
    "warp/accepted-tos.txt".text = "yes\n";
  };
}
