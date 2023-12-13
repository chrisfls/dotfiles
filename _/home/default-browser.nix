{ config, pkgs, lib, ... }:
with lib;
let
  cfg = config.module.default-browser;
in
{
  options.module.default-browser = {
    enable = mkEnableOption "default-browser module";
    name = mkOption { type = types.str; };
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      xdg-utils
      desktop-file-utils
    ];

    xdg = {
      enable = true;
      mimeApps = {
        enable = true;
        defaultApplications = {
          "x-scheme-handler/http" = cfg.name;
          "x-scheme-handler/https" = cfg.name;
          "x-scheme-handler/chrome" = cfg.name;
          "text/html" = cfg.name;
          "application/x-extension-htm" = cfg.name;
          "application/x-extension-html" = cfg.name;
          "application/x-extension-shtml" = cfg.name;
          "application/xhtml+xml" = cfg.name;
          "application/x-extension-xhtml" = cfg.name;
          "application/x-extension-xht" = cfg.name;
        };
      };
    };
  };
}
