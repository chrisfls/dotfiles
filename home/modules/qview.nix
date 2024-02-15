{ config, lib, pkgs, ... }:
let
  inherit (config.modules.qview) enable;

  desktop = "com.interversehq.qView.desktop";
in
{
  options.modules.qview.enable = lib.mkEnableOption "Enable qview module";
  config = lib.mkIf enable {
    pacman.packages = [ "chaotic-aur/qview" ];

    xdg.mimeApps.defaultApplications = {
      "image/*" = desktop;
      "image/bmp" = desktop;
      "image/gif" = desktop;
      "image/jpeg" = desktop;
      "image/png" = desktop;
      "image/svg+xml" = desktop;
      "image/tiff" = desktop;
      "image/webp" = desktop;
      "image/x-icon" = desktop;
    };
  };
}
