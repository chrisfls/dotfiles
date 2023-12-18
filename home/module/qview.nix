{ config, lib, pkgs, ... }:
let
  cfg = config.extra.qview;

  desktop = "com.interversehq.qView.desktop";
in
{
  options.extra.qview.enable = lib.mkEnableOption "Enable qview module";

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.qview ];

    xdg.mimeApps = {
      enable = true;
      defaultApplications = {
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
  };
}
