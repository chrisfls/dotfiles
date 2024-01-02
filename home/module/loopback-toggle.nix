{ config, lib, pkgs, ... }:
let
  inherit (config.module.loopback-toggle)
    device
    enable
    volume;

  amixer =
    "${pkgs.alsa-utils}/bin/amixer";

  loopback-toggle = pkgs.writeShellScriptBin "loopback-toggle"
    ''
      d=$(awk '/${device}/ {print $1; exit}' /proc/asound/cards)
      ${amixer} -c $d sset Mic Capture 100%
      ${amixer} -c $d sset Mic Playback ${toString volume}%
      ${amixer} -c $d sset Mic Playback toggle
    '';

  loopback-offd = pkgs.writeShellScriptBin "loopback-offd"
    ''
      ${loopback-toggle}/bin/loopback-toggle
      pactl subscribe | grep --line-buffered "Event 'change' on source" | while read line
      do
        d=$(awk '/${device}/ {print $1; exit}' /proc/asound/cards)
        ${amixer} -c $d sset Mic Capture 100%
        ${amixer} -c $d sset Mic Playback ${toString volume}%
        ${amixer} -c $d sset Mic Playback off
      done
    '';
in
{
  options.module.loopback-toggle = {
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
    # module.sxhkd.keybindings = {
    #  # TODO: map to F24 (keycode: 202) 
    #   "super + Pause" = "${loopback-toggle}/bin/loopback-toggle";
    # };

    xsession.windowManager.i3.config.keybindings."Mod4+Pause" = "${loopback-toggle}/bin/loopback-toggle";

    systemd.user.services.loopback-offd = {
      Unit.Description = "Disables audio loopback by default";
      Service.ExecStart = "${loopback-offd}/bin/loopback-offd";
    };
  };
}
