{ config, lib, pkgs, ... }:
let
  cfg = config.extra.picom;
in
{
  options.extra.picom.enable = lib.mkEnableOption "Enable picom module";

  config = lib.mkIf cfg.enable {
    services.picom = {
      enable = true;
      package = pkgs.picom-next;
      backend = "glx";
      extraArgs = [
        "--vsync-use-glfinish"
        "--glx-no-stencil"
        # "--no-use-damage"
      ];
      fade = true;
      fadeDelta = 16;
      fadeSteps = [ 0.2 0.2 ];
      shadow = true;
      shadowOffsets = [ (-18) (-16) ];
      shadowOpacity = 0.5;
      vSync = true;
      settings = {
        corner-radius = 12;
        crop-shadow-to-monitor = true;
        blur = {
          method = "dual_kawase";
          strength = 4;
          background = false;
          background-frame = false;
          background-fixed = false;
        };
        corner-radius-rules = [
          "12:class_g = 'Rofi'"
        ];
        rounded-corners-exclude = [
          "class_g = 'Polybar'"
        ];
      };
      wintypes = {
        normal = { blur-background = true; shadow = false; };
        splash = { blur-background = false; shadow = false; };
      };
    };
  };
}
