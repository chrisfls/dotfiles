{ config, inputs, lib, pkgs, ... }:
let inherit (config.module.cloudflare-warp) enable; in {
  options.module.cloudflare-warp.enable = lib.mkEnableOption "Enable cloudflare-warp module";

  config = lib.mkIf enable {
    pacman.packages = [ "chaotic-aur/cloudflare-warp-bin" ];

    xdg.dataFile = {
      "warp/accepted-teams-tos.txt".text = "yes\n";
      "warp/accepted-tos.txt".text = "yes\n";
    };
  };
}
