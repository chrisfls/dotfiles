# commands:

{ config, lib, pkgs, ... }:
let
  cfg = config.extra.loopback-toggle;

  loopback-toggle = pkgs.writeShellScriptBin "loopback-toggle"
    ''
      d=$(awk '/${cfg.device}/ {print $1; exit}' /proc/asound/cards)
      amixer -c $d sset Mic Capture 100%
      amixer -c $d sset Mic Playback ${toString cfg.volume}%
      amixer -c $d sset Mic Playback toggle
    '';
in
{
  options.extra.loopback-toggle = {
    enable = lib.mkEnableOption "Enable loopback-toggle module";
    volume = lib.mkOption {
      type = lib.types.int;
      default = 2;
    };
    device = lib.mkOption {
      type = lib.types.string;
      default = "ARCANO MARK-HI";
    };
  };

  config = lib.mkIf cfg.enable {
    services.sxhkd.keybindings = {
      # TODO: map to F24 (keycode: 202) 
      "super + Pause" = "${loopback-toggle}/bin/loopback-toggle";
    };
  };
}
