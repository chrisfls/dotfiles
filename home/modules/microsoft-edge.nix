{ config, lib, pkgs, specialArgs, ... }:
let
  inherit (config.modules.microsoft-edge) enable;

  exe = "microsoft-edge-stable";

  pkg =
    pkgs.writeShellScriptBin exe
      ''
        exec /usr/bin/microsoft-edge-stable --force-device-scale-factor=1.5 --enable-features=VaapiVideoDecodeLinuxGL "$@"
      '';
in
{
  options.modules.microsoft-edge.enable = lib.mkEnableOption "Enable microsoft-edge module";

  config = lib.mkIf enable {
    home.packages = [ pkg ];
    modules.i3wm.apps."shift+b" = exe;

    xdg.desktopEntries = {
      "microsoft-edge" = {
        name = "Microsoft Edge";
        type = "Application";
        genericName = "Web Browser";
        comment = "Access the Internet";
        categories = [ "Network" "WebBrowser" ];
        icon = "microsoft-edge";
        exec = "microsoft-edge-stable %U";
        noDisplay = false;
        startupNotify = true;
        terminal = false;
      };
    };
  };
}
