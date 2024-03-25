{ config, lib, pkgs, specialArgs, ... }:
let
  inherit (config.modules.microsoft-edge) enable;

  exe = "microsoft-edge-stable";
in
{
  options.modules.microsoft-edge.enable = lib.mkEnableOption "Enable microsoft-edge module";

  config = lib.mkIf enable {
    pacman.packages = [
      "chaotic-aur/microsoft-edge-stable-bin"
      "aur/microsoft-edge-beta-bin"
    ];

    home.packages = [
      (pkgs.writeHostScriptBin exe
        ''
          exec /usr/bin/${exe} --ozone-platform=wayland --enable-features=VaapiVideoDecodeLinuxGL "$@"
        '')

      (pkgs.writeHostScriptBin "microsoft-edge-beta"
        ''
          exec /usr/bin/microsoft-edge-beta --ozone-platform=wayland --enable-features=VaapiVideoDecodeLinuxGL "$@"
        '')
    ];

    #modules.sway.apps."shift+b" = exe;
  };
}
