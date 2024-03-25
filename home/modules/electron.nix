{ config, lib, ... }:
let
  inherit (config.modules.electron) enable;

  flags =
    ''
      --enable-features=UseOzonePlatform,WaylandWindowDecorations,WebRTCPipeWireCapturer
      --ozone-platform=wayland
    '';
in
{
  options.modules.electron.enable = lib.mkEnableOption "Enable electron module";

  config = lib.mkIf enable {
    xdg.configFile = {
      "electron28-flags.conf".text = flags;
      "electron29-flags.conf".text = flags;
      "logseq-flags.conf".text = flags;
    };
  };
}
