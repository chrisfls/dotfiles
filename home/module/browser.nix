{ config, lib, pkgs, ... }:
let
  cfg = config.extra.browser;
in
{
  options.extra.browser.enable = lib.mkEnableOption "Enable browser module";

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.brave ];

    extra.nixGL.overlay.brave = [ "brave" ];

    /* 
          "x-scheme-handler/http" = cfg.name;
          "x-scheme-handler/https" = cfg.name;
          "x-scheme-handler/chrome" = cfg.name;
          "text/html" = cfg.name;
          "application/x-extension-htm" = cfg.name;
          "application/x-extension-html" = cfg.name;
          "application/x-extension-shtml" = cfg.name;
          "application/xhtml+xml" = cfg.name;
          "application/x-extension-xhtml" = cfg.name;
          "application/x-extension-xht" = cfg.name; */
  };
}
