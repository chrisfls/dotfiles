{ config, lib, pkgs, ... }:
let
  inherit (config.module.qview) enable;

  desktop = "com.interversehq.qView.desktop";
in
{
  options.module.qview.enable = lib.mkEnableOption "Enable qview module";

  config = lib.mkIf enable {
    home.packages = [ pkgs.usr.qview ];
    pacman.usr.qview = [ "chaotic-aur/qview" ];

    xdg.mimeApps.defaultApplications = {
      "image/jpeg" = desktop;
      "image/png" = desktop;
      "image/gif" = desktop;
      "image/bmp" = desktop;
      "image/tiff" = desktop;
      "image/webp" = desktop;
      "image/svg+xml" = desktop;
      "image/x-icon" = desktop;
    };
  };
}
