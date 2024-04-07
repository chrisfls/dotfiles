{ config, lib, pkgs, ... }:
let inherit (config.modules.rclone) enable; in {
  options.modules.rclone.enable = lib.mkEnableOption "Enable rclone module";

  config = lib.mkIf enable {
    pacman.packages = [ "extra/rclone" ];

    home.packages = [
      (pkgs.writeHostScriptBin "rclone-sync" (lib.readFile ../../assets/rclone-sync.sh))
    ];
  };
}
