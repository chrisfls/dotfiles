{ config, lib, pkgs, ... }:
let inherit (config.modules.retroarch) enable; in
{
  options.modules.retroarch.enable = lib.mkEnableOption "Enable retroarch module";
  config = lib.mkIf enable {
    pacman.packages = [
      "extra/retroarch"
      "extra/retroarch-assets-ozone"
    ];
  };
}
