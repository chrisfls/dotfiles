{ config, inputs, lib, pkgs, ... }:
let inherit (config.modules.cloudflare-warp) enable; in {
  options.modules.cloudflare-warp.enable = lib.mkEnableOption "Enable cloudflare-warp module";

  config = lib.mkIf enable {
    pacman.packages = [
      "chaotic-aur/cloudflare-warp-bin"
      "aur/cloudflared-bin"
    ];

    xdg.dataFile = {
      "warp/accepted-teams-tos.txt".text = "yes\n";
      "warp/accepted-tos.txt".text = "yes\n";
    };
  };
}
