{ config, lib, pkgs, specialArgs, ... }:
let
  inherit (config.presets) archlinux;
  inherit (config.modules.microsoft-edge) enable;

  exe =
    if archlinux then
      "microsoft-edge-stable"
    else
      "microsoft-edge";

  pkg =
    if archlinux then
      pkgs.writeShellScriptBin exe
        ''
          exec /usr/bin/microsoft-edge-stable --force-device-scale-factor=1.5 --enable-features=VaapiVideoDecodeLinuxGL "$@"
        ''

    else
      pkgs.microsoft-edge.overrideAttrs
        (old: {
          postFixup =
            ''
              substituteInPlace $out/bin/microsoft-edge \
                --replace "--enable-features=" "--force-device-scale-factor=1.5 --enable-features=VaapiVideoDecodeLinuxGL,"
            '';
        });
in
{
  options.modules.microsoft-edge.enable = lib.mkEnableOption "Enable microsoft-edge module";

  config = lib.mkIf enable {
    home.packages = [ pkg ];
    modules.i3wm.apps."shift+b" = exe;

    xdg.desktopEntries = lib.mkIf archlinux {
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
