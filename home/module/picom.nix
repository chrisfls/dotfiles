{ config, lib, pkgs, specialArgs, ... }:
let inherit (config.module.picom) enable; in
{
  options.module.picom.enable = lib.mkEnableOption "Enable picom module";

  config = lib.mkIf enable {
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
        vsync = false;

        # disable shadows
        shadow = true;
        crop-shadow-to-monitor = true;
        shadow-offset-x = -18;
        shadow-offset-y = -18;
        shadow-opacity = 0.500000;
        shadow-exclude = [
          "!(class_g = 'Rofi' || class_g = 'Polybar' || class_g = 'Dunst')"
        ];

        # border opacity
        frame-opacity = 0.85;
        opacity-rule = [
          "0:_NET_WM_STATE@[0]:32a *= '_NET_WM_STATE_HIDDEN'"
          "0:_NET_WM_STATE@[1]:32a *= '_NET_WM_STATE_HIDDEN'"
          "0:_NET_WM_STATE@[2]:32a *= '_NET_WM_STATE_HIDDEN'"
          "0:_NET_WM_STATE@[3]:32a *= '_NET_WM_STATE_HIDDEN'"
          "0:_NET_WM_STATE@[4]:32a *= '_NET_WM_STATE_HIDDEN'"
          "0:_COMPTON_MONOCLE@:32c = 0"
        ];

        # speedup games:
        unredir-if-possible = true;

        # fade animations -------------
        fade-delta = 8;
        fade-exclude = [ ];
        fade-in-step = 0.250000;
        fade-out-step = 0.250000;
        fading = true;

        # rounded corners -------------
        corner-radius = 2;
        # force round corners for rofi and Dunst
        corner-radius-rules = [
          "12:class_g = 'Rofi'"
          "12:class_g = 'Dunst'"
        ];
        # force straight corners for polybar
        rounded-corners-exclude = [ "class_g = 'Polybar'" ];

        # blurred backgrounds ---------
        blur-method = "dual_kawase";
        blur-background = true;
        blur-background-fixed = true;
        blur-background-frame = false;
        blur-strength = 4;
        # only blur rofi, dunst and alacritty
        blur-background-exclude = "!(class_g = 'Rofi' || class_g = 'Alacritty' || class_g = 'org.wezfurlong.wezterm' || class_g = 'Dunst')";
      };
    };
  };
}
