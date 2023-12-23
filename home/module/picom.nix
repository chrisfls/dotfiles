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

        # fade animations -------------
        fade-delta = 8;
        fade-exclude = [ ];
        fade-in-step = 0.250000;
        fade-out-step = 0.250000;
        fading = true;

        # shadows ---------------------
        shadow = false;
        crop-shadow-to-monitor = true;
        shadow-exclude = [
          "_GTK_FRAME_EXTENTS@:c"
          "(bounding_shaped && !rounded_corners)"
          "class_g ?= 'Notify-osd'"
          "class_g ?= 'zoom'"
          "class_g = 'Cairo-clock'"
          "class_g = 'Conky'"
          "name = 'cpt_frame_xcb_window'"
          "name = 'Notification'"
          "name = 'rect-overlay'"
        ];
        shadow-offset-x = -18;
        shadow-offset-y = -18;
        shadow-opacity = 0.500000;

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

      # only enable shadows for normal windows and for polybar
      wintypes = {
        dock.shadow = true;
        normal.shadow = true;
      };
    };
  };
}
