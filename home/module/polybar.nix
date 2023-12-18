{ config, lib, pkgs, ... }:
let
  cfg = config.extra.polybar;
in
{
  options.extra.polybar.enable = lib.mkEnableOption "Enable polybar module";

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.polybar ];
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
  };

}
