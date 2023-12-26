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

    xsession.windowManager.bspwm = {
      enable = true;
      package = pkgs.bspwm.overrideAttrs (old: {
        src = pkgs.fetchFromGitHub {
          owner = "baskerville";
          repo = "bspwm";
          rev = "af3bd8b4351f4478fe0fe3cfd6c09e44cb108b4b";
          hash = "sha256-O4Qwdjb3p6jw8Qtcd4zGZ57cB3oCCbPZcjUQtWbyC7Y=";
        };
      });
      rules = {
        "Yad".floating = true;
        "copyq" = {
          state = "floating";
          rectangle = "1280x960+0+0";
          center = true;
        };
        "qalculate-qt" = {
          state = "floating";
          rectangle = "768x640+0+0";
          center = true;
        };
        "pavucontrol-qt" = {
          state = "floating";
          rectangle = "1115x839+0+0";
          center = true;
        };
        "bluedevil-wizard" = {
          state = "floating";
          rectangle = "750x678+0+0";
          center = true;
        };
      };
      settings = {
        border_width = 2;
        window_gap = 18;

        # Borders and gaps
        borderless_monocle = true;
        gapless_monocle = true;
        single_monocle = true;

        # Border colors
        normal_border_color = colors.background;
        focused_border_color = colors.foreground;
        borderless_singleton = true;

        # disble multihead behavior (TODO: move to user)
        remove_disabled_monitors = true;
        remove_unplugged_monitors = true;

        # Focusing behavior
        focus_follows_pointer = false;

        # Misc
        split_ratio = 0.50;
        initial_polarity = "second_child";
        automatic_scheme = "alternate";
      };
    };
  };
}
