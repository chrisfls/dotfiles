{ config, lib, ... }:
let
  cfg = config.extra.bar;
in
{
  options.extra.bar.enable = lib.mkEnableOption "Enable bar module";

  config = lib.mkIf cfg.enable { };

  # TODO: polybar config here
  /*services.polybar = {
    enable = true;
    settings = {
      "module/volume" = {
        type = "internal/pulseaudio";
        format.volume = "<ramp-volume> <label-volume>";
        label.muted.text = "🔇";
        label.muted.foreground = "#666";
        ramp.volume = [ "🔈" "🔉" "🔊" ];
        click.right = "pavucontrol &";
      };
    };
  };*/
}
