{ config, lib, pkgs, ... }:
let
  inherit (config.modules.loopback-toggle) device enable volume;

  loopback-toggle = 
      (pkgs.writeHostScriptBin "loopback-toggle"
        ''
          d=$(awk '/${device}/ {print $1; exit}' /proc/asound/cards)
          amixer -c $d sset Mic Capture 100%
          amixer -c $d sset Mic Playback ${toString volume}%
          amixer -c $d sset Mic Playback toggle
        '');

  loopback-offd = pkgs.writeScript "loopback-offd"
    ''
      ${loopback-toggle}/bin/loopback-toggle
      pactl subscribe | grep --line-buffered "Event 'change' on source" | while read line
      do
        d=$(awk '/${device}/ {print $1; exit}' /proc/asound/cards)
        amixer -c $d sset Mic Capture 100%
        amixer -c $d sset Mic Playback ${toString volume}%
        amixer -c $d sset Mic Playback off
      done
    '';
in
{
  options.modules.loopback-toggle = {
    enable = lib.mkEnableOption "Enable loopback-toggle module";

    volume = lib.mkOption {
      type = lib.types.int;
      default = 2;
    };

    device = lib.mkOption {
      type = lib.types.str;
      default = "ARCANO MARK-HI";
    };
  };

  config = lib.mkIf enable {
    pacman.packages = ["extra/alsa-utils" "extra/pamixer"];

    home.packages = [ loopback-toggle ];

    systemd.user.services.loopback-offd = {
      Unit.Description = "Disables audio loopback by default";
      Service.ExecStart = "exec \"${loopback-offd}\"";
    };
  };
}
