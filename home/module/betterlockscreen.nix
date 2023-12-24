# update cache with
# $ betterlockscreen -u /home/kress/Pictures/wallpaper/23-12-23.png --fx dim,blur
{ config, lib, pkgs, ... }:
let
  cfg = config.module.betterlockscreen;
in
{
  options.module.betterlockscreen.enable = lib.mkEnableOption "Enable betterlockscreen module";

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.feh ];

    services.betterlockscreen = {
      enable = true;
      arguments = [ "blur" ];
    };
  };
}
