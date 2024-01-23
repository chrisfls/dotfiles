{ config, lib, pkgs, specialArgs, ... }:
let
  inherit (config.modules.picom) enable;
  inherit (config.presets) non-nixos;
in
{
  options.modules.picom.enable = lib.mkEnableOption "Enable picom module";

  config = lib.mkIf enable {
    home.packages =
      if non-nixos then
        [ ]

      else
        [ pkgs.picom-next ];

    pacman.packages =
      if non-nixos then
        [ "chaotic-aur/picom-git" ]

      else
        [ ];

    systemd.user.services.picom = {
      Unit = {
        Description = "Picom X11 compositor";
        After = [ "graphical-session-pre.target" ];
        PartOf = [ "graphical-session.target" ];
      };

      Install = { WantedBy = [ "graphical-session.target" ]; };

      Service = {
        ExecStart =
          let
            scriptPkg = pkgs.writeShellScriptBin "picom-start"
              "picom --config ${config.xdg.configHome}/picom/picom.conf --vsync-use-glfinish --glx-no-stencil";
          in
          "${scriptPkg}/bin/picom-start";
        Restart = "always";
        RestartSec = 3;
      };
    };

    xdg.configFile."picom/picom.conf".text =
      ''
        backend = "glx";
        vsync = false;

        # speedup games:
        unredir-if-possible = true;

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
        frame-opacity = 0.850000;
        # inactive-opacity = 1.000000;
        opacity-rule = [
          "0:_NET_WM_STATE@[0]:32a *= '_NET_WM_STATE_HIDDEN'",
          "0:_NET_WM_STATE@[1]:32a *= '_NET_WM_STATE_HIDDEN'",
          "0:_NET_WM_STATE@[2]:32a *= '_NET_WM_STATE_HIDDEN'",
          "0:_NET_WM_STATE@[3]:32a *= '_NET_WM_STATE_HIDDEN'",
          "0:_NET_WM_STATE@[4]:32a *= '_NET_WM_STATE_HIDDEN'",
          "0:_COMPTON_MONOCLE@:32c = 0"
        ];

        # fade animations -------------
        fade-delta = 8;
        fade-exclude = [  ];
        fade-in-step = 0.250000;
        fade-out-step = 0.250000;
        fading = true;

        # rounded corners -------------
        corner-radius = 2;
        # force round corners for rofi and Dunst
        corner-radius-rules = [
          "12:class_g = 'Rofi'",
          "12:class_g = 'Dunst'"
        ];
        # force straight corners for polybar
        rounded-corners-exclude = [ "class_g = 'Polybar'" ];

        blur-method = "dual_kawase";
        blur-background = true;
        blur-background-fixed = true;
        blur-background-frame = false;
        blur-strength = 4;

        # only blur rofi, dunst and terminals
        blur-background-exclude = "!(
          class_g = 'Rofi' ||
          class_g = 'Alacritty' ||
          class_g = 'org.wezfurlong.wezterm' ||
          class_g = 'kitty' ||
          class_g = 'Dunst'
        )";

        # wintypes: { dropdown_menu = { opacity = 1.000000; }; popup_menu = { opacity = 1.000000; }; };⏎        
      '';
  };
}
