{ config, lib, pkgs, ... }:
let
  cfg = config.module.bspwm;

  colors = config.module.themes.color-scheme;
in
{
  options.module.bspwm.enable = lib.mkEnableOption "Enable bspwm module";

  config = lib.mkIf cfg.enable {
    home.sessionVariables.DESKTOP_SESSION = "bspwm";

    xsession.initExtra = "systemctl --user import-environment DESKTOP_SESSION";

    xsession.windowManager.bspwm.startupPrograms = [
      "systemd-cat -t pcmanfm-qt systemd-run --user --scope --property=OOMPolicy=continue -u pcmanfm-qt ${pkgs.lxqt.pcmanfm-qt}/bin/pcmanfm-qt --desktop"
      "systemd-cat -t copyq systemd-run --user --scope --property=OOMPolicy=continue -u copyq ${pkgs.copyq}/bin/copyq"
      "systemd-cat -t nm-tray systemd-run --user --scope --property=OOMPolicy=continue -u nm-tray ${pkgs.nm-tray}/bin/nm-tray"
    ];

    xsession.windowManager.bspwm = {
      enable = true;
      rules = {
        "Yad".floating = true;
      };
      settings = {
        border_width = 2;
        window_gap = 16;

        # Borders and gaps
        borderless_monocle = true;
        gapless_monocle = true;
        single_monocle = true;

        # Border colors
        normal_border_color = colors.background;
        focused_border_color = colors.foreground;

        # disble multihead behavior (TODO: move to user)
        remove_disabled_monitors = true;
        remove_unplugged_monitors = true;

        # Focusing behavior
        focus_follows_pointer = false;

        # Misc
        split_ratio = 0.50;
        initial_polarity = "second_child";
      };
    };
  };
}
