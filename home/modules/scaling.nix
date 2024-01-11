{ config, lib, pkgs, specialArgs, ... }:
let
  inherit (config.modules.scaling)
    dpi
    enable
    scale;

  dpi-scaled = builtins.floor (dpi * scale);
  # not using this anymore, better having more space
  # gdk-scale = builtins.ceil scale;
  # gdk-dpi-scale = 1.0 / gdk-scale;
in
{
  options.modules.scaling = {
    enable = lib.mkEnableOption "Enable scaling module";

    scale = lib.mkOption {
      type = lib.types.float;
      default = 1;
    };

    dpi = lib.mkOption {
      type = lib.types.int;
      default = 96;
    };

    dpi-scaled = lib.mkOption {
      type = lib.types.int;
    };
  };

  # when using GDK/QT scaling, the DPI must be hardcoded
  config = lib.mkIf enable {
    modules.scaling.dpi-scaled = lib.mkForce dpi-scaled;

    xresources.properties."Xft.dpi" = toString dpi-scaled;

    home.sessionVariables = {
      QT_AUTO_SCREEN_SCALE_FACTOR = toString 0;
      QT_ENABLE_HIGHDPI_SCALING = toString 1;
      QT_SCALE_FACTOR = toString scale;

      QT_FONT_DPI = toString dpi;

      # double size of icons (slows down gtk3 rendering a bit)
      # GDK_SCALE = toString gdk-scale;
      # GDK_DPI_SCALE = toString gdk-dpi-scale;
    };
  };
}
