{ config, lib, pkgs, ... }:
let
  cfg = config.module.picom;
in
{
  options.module.picom.enable = lib.mkEnableOption "Enable picom module";

  config = lib.mkIf cfg.enable {
    services.picom = {
      enable = true;
      package = pkgs.picom-next;
      extraArgs = [
        "--vsync-use-glfinish"
        "--glx-no-stencil"
      ];

      # general settings
      settings = {
        backend = "glx";
        vsync = true;

        # disable shadows
        shadow = false;

        # fade animations -------------
        fade-delta = 8;
        fade-exclude = [ ];
        fade-in-step = 0.250000;
        fade-out-step = 0.250000;
        fading = true;

        # rounded corners -------------
        corner-radius = 12;
        # force round corners for rofi
        corner-radius-rules = [ "12:class_g = 'Rofi'" ];
        # force straight corners for polybar
        rounded-corners-exclude = [ "class_g = 'Polybar'" ];

        # blurred backgrounds ---------
        blur-method = "dual_kawase";
        blur-background = true;
        blur-background-fixed = true;
        blur-background-frame = false;
        blur-strength = 4;
        # only blur rofi and alacritty
        blur-background-exclude = "!(class_g = 'Rofi' || class_g = 'Alacritty')";
      };
    };
  };
}
