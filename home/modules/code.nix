{ config, lib, pkgs, ... }:
let inherit (config.modules.code) enable; in {
  options.modules.code.enable = lib.mkEnableOption "Enable code module";

  config = lib.mkIf enable {
    pacman.packages = [ "chaotic-aur/visual-studio-code-bin" ];

    xdg.configFile = {
      "code-flags.conf".text =
        ''
          --enable-features=UseOzonePlatform,WaylandWindowDecorations,WebRTCPipeWireCapturer
          --ozone-platform=wayland
        '';
    };
  };
}
