{ config, lib, pkgs, specialArgs, ... }:
let
  inherit (config.modules.microsoft-edge) enable;

  exe = "microsoft-edge-stable";
in
{
  options.modules.microsoft-edge.enable = lib.mkEnableOption "Enable microsoft-edge module";

  config = lib.mkIf enable {
    # pacman.packages = [ "chaotic-aur/${exe}-bin" ];

    # TODO: check if flatpak is using this
    xdg.configFile."${exe}-flags.conf".text =
      ''
        --enable-features=WebRTCPipeWireCapturer,VaapiVideoDecodeLinuxGL
      '';
  };
}
