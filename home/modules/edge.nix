{ config, lib, pkgs, specialArgs, ... }:
let inherit (config.modules.edge) enable; in {
  options.modules.edge.enable = lib.mkEnableOption "Enable edge module";

  config = lib.mkIf enable {
    pacman.packages = [
      "chaotic-aur/microsoft-edge-stable-bin"
      "aur/microsoft-edge-beta-bin"
    ];
  };
}
