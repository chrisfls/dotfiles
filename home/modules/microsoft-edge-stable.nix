{ config, lib, pkgs, specialArgs, ... }:
let
  inherit (config.modules.microsoft-edge-stable) enable;

  exe = "microsoft-edge-stable";
in
{
  options.modules.microsoft-edge-stable.enable = lib.mkEnableOption "Enable microsoft-edge module";

  config = lib.mkIf enable {
    pacman.packages = [ "chaotic-aur/${exe}-bin" ];

    xdg.configFile."${exe}-flags.conf".text =
      ''
        --enable-features=UseOzonePlatform,WaylandWindowDecorations,WebRTCPipeWireCapturer,VaapiVideoDecodeLinuxGL
        --ozone-platform=wayland
      '';
  };
}
