{ config, lib, pkgs, specialArgs, ... }:
let
  cfg = config.module.scaling;

  scale = builtins.ceil cfg.scale;
  gdk-dpi-scale = cfg.scale / scale;
in
{
  options.module.scaling = {
    enable = lib.mkEnableOption "Enable scaling module";

    scale = lib.mkOption {
      type = lib.types.float;
      default = 1;
    };

    dpi = lib.mkOption {
      type = lib.types.int;
      default = 96;
    };

    dpiScaled = lib.mkOption {
      type = lib.types.int;
      default = cfg.dpi * scale;
    };
  };

  # when using GDK/QT scaling, the DPI must be hardcoded
  config = lib.mkIf cfg.enable {
    xresources.properties."Xft.dpi" = toString config.module.scaling.dpiScaled;

    home.sessionVariables = {
      GDK_SCALE = toString 1;
      GDK_DPI_SCALE = toString gdk-dpi-scale;

      QT_AUTO_SCREEN_SCALE_FACTOR = toString 0;
      QT_ENABLE_HIGHDPI_SCALING = toString 1;
      QT_SCALE_FACTOR = toString cfg.scale;

      QT_FONT_DPI = toString cfg.dpi;
    };
  };
}
