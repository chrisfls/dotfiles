{ config, lib, ... }:
let enable = config.presets.stream; in
{
  options.presets.stream = lib.mkEnableOption "Enable stream preset";

  config = lib.mkIf enable {
    presets.desktop = true;
    modules = {
      obs.enable = true;
      microsoft-edge-beta.enable = true;
    };
  };
}
