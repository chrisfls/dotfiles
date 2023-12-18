{ config, lib, pkgs, specialArgs, ... }:
let
  cfg = config.extra.scaling;

  scale = cfg.scale;

  gdk-scale = builtins.ceil scale;

  gdk-dpi-scale = scale / gdk-scale;
in
{
  options.extra.scaling = {
    enable = lib.mkEnableOption "Enable scaling module";

    scale = lib.mkOption {
      type = lib.types.float;
      default = 1;
    };

    dpi = lib.mkOption {
      type = lib.types.int;
      default = 96;
    };
  };

  # when using GDK/QT scaling, the DPI must be hardcoded
  config = lib.mkIf cfg.enable {
    xresources.properties."Xft.dpi" = cfg.dpi;

    home.sessionVariables = {
      GDK_SCALE = gdk-scale;
      GDK_DPI_SCALE = gdk-dpi-scale;

      QT_AUTO_SCREEN_SCALE_FACTOR = 0;
      QT_ENABLE_HIGHDPI_SCALING = 1;
      QT_SCALE_FACTOR = scale;

      QT_FONT_DPI = cfg.dpi;
    };
  };
}
