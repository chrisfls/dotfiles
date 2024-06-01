# TODO: deprecate
{ config, lib, pkgs, specialArgs, ... }:
let
  inherit (config.modules.microsoft-edge-beta) enable;

  exe = "microsoft-edge-beta";
in
{
  options.modules.microsoft-edge-beta.enable = lib.mkEnableOption "Enable microsoft-edge-beta module";

  config = lib.mkIf enable {
    pacman.packages = [ "aur/${exe}-bin" ];

    xdg.configFile."${exe}-flags.conf".text =
      ''
        --enable-features=UseOzonePlatform,WaylandWindowDecorations,WebRTCPipeWireCapturer,VaapiVideoDecodeLinuxGL
        --ozone-platform=wayland
      '';
  };
}
