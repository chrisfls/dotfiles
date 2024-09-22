{ config, lib, ... }:
let inherit (config.modules.brave) enable; in {
  options.modules.brave.enable = lib.mkEnableOption "Enable brave module";

  config = lib.mkIf enable {
    # pacman.packages = [ "chaotic-aur/brave-bin" ];

    # TODO: check if flatpak is using this
    xdg.configFile."brave-flags.conf".text =
      ''
        --enable-features=WebRTCPipeWireCapturer,VaapiVideoDecodeLinuxGL
      '';
  };
}
