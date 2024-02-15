{ config, lib, pkgs, specialArgs, ... }:
let
  inherit (config.modules.brave) enable;

  pkg = pkgs.writeShellScriptBin "brave"
    ''
      exec /usr/bin/brave --force-device-scale-factor=1.5 --enable-features=VaapiVideoDecodeLinuxGL "$@"
    '';
in
{
  options.modules.brave.enable = lib.mkEnableOption "Enable brave module";

  config = lib.mkIf enable {
    home.packages = [ pkg ];
    pacman.packages = [ "chaotic-aur/brave-bin" ];

    xdg.mimeApps.defaultApplications =
      let desktop = "brave-browser.desktop";
      in {
        "application/pdf" = desktop;
        "application/x-extension-htm" = desktop;
        "application/x-extension-html" = desktop;
        "application/x-extension-shtml" = desktop;
        "application/x-extension-xht" = desktop;
        "application/x-extension-xhtml" = desktop;
        "application/xhtml+xml" = desktop;
        "text/html" = desktop;
        "x-scheme-handler/about" = desktop;
        "x-scheme-handler/chrome" = desktop;
        "x-scheme-handler/ftp" = desktop;
        "x-scheme-handler/http" = desktop;
        "x-scheme-handler/https" = desktop;
        "x-scheme-handler/unknown" = desktop;
      };

    modules.i3wm.apps."b" = "brave-browser";
  };
}
