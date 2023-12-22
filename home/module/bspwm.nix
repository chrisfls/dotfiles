{ config, lib, pkgs, ... }:
let
  cfg = config.extra.bspwm;
in
{
  options.extra.bspwm.enable = lib.mkEnableOption "Enable bspwm module";

  config = lib.mkIf cfg.enable {
    home.sessionVariables.DESKTOP_SESSION = "bspwm";

    xsession.initExtra = "systemctl --user import-environment DESKTOP_SESSION";

    xsession.windowManager.bspwm.startupPrograms = [
      "systemd-cat -t pcmanfm-qt-desktop systemd-run --user --scope --property=OOMPolicy=continue -u pcmanfm-qt-desktop ${pkgs.lxqt.pcmanfm-qt}/bin/pcmanfm-qt --desktop"
      "systemd-cat -t copyq systemd-run --user --scope --property=OOMPolicy=continue -u copyq ${pkgs.copyq}/bin/copyq"
    ];

    xsession.windowManager.bspwm = {
      enable = true;
      monitors = {
        HDMI-1 = [ "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" ];
      };
      rules = { };
      settings = {
        # top_padding = 24;
        # bottom_padding = 0;
        # left_padding = 0;
        # right_padding = 0;
        border_width = 2;
        #window_gap = 16;

        # Borders and gaps
        borderless_monocle = true;
        gapless_monocle = true;

        # Border colors
        # normal_border_color = "#484848";
        # focused_border_color = "#1F8999";
        # urgent_border_color = "#B58900";
        # presel_border_color = "#26E2FF";

        # It looks like these options get overridden by Compton...
        # normal_frame_opacity = 0.75;
        # focused_frame_opacity = 0.75;

        # Multihead behavior
        # remove_disabled_monitors = true;
        # remove_unplugged_monitors = true;

        # Focusing behavior
        # focus_follows_pointer = false;
        # history_aware_focus = true;
        # focus_by_distance = true;

        # Misc
        # split_ratio = 0.50;
        # auto_alternate = true;
        # auto_cancel = true;
        # initial_polarity = first_child;
      };


      # Set up displays
      # bspc monitor -d 1 2 3 4

      # bspc rule -a Conky sticky=on manage=off lower=on
      # bspc rule -a xfce4-session floating=on fullscreen=on
      # bspc rule -a xfce4-panel floating=on manage=off
      # bspc rule -a wrapper-1.0 floating=on border=off focus=on
      # bspc rule -a Firefox desktop=^4 focus=on
      # bspc rule -a Thunderbird floating=on
      # bspc rule -a Yad floating=on
    };
  };
}
