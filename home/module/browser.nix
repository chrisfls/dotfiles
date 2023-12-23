{ config, lib, pkgs, ... }:
let
  cfg = config.module.browser;

  browser = "brave-browser.desktop";
in
{
  options.module.browser.enable = lib.mkEnableOption "Enable browser module";

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.brave ];

    xdg.mimeApps = {
      enable = true;
      defaultApplications = {
        "x-scheme-handler/http" = browser;
        "x-scheme-handler/https" = browser;
        "x-scheme-handler/chrome" = browser;
        "text/html" = browser;
        "application/x-extension-htm" = browser;
        "application/x-extension-html" = browser;
        "application/x-extension-shtml" = browser;
        "application/xhtml+xml" = browser;
        "application/x-extension-xhtml" = browser;
        "application/x-extension-xht" = browser;
      };
    };
  };
}
