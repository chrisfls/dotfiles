{ config, lib, pkgs, specialArgs, ... }:
let
  inherit (config.modules.themes)
    cursor
    enable
    font
    gtk
    icon
    qt;

  inherit (config.modules.scaling) scale;

  toINI = lib.generators.toINI { };
in
{
  options.modules.themes = {
    enable = lib.mkEnableOption "Enable themes module";

    qt = {
      style = lib.mkOption {
        type = lib.types.str;
        default = "breeze";
      };

      package = lib.mkOption {
        type = lib.types.package;
        default = pkgs.materia-kde-theme;
      };
    };

    gtk = {
      name = lib.mkOption {
        type = lib.types.str;
        default = "Breeze-Dark";
      };

      package = lib.mkOption {
        type = lib.types.package;
        default = pkgs.libsForQt5.breeze-gtk;
      };
    };

    icon = {
      name = lib.mkOption {
        type = lib.types.str;
        default = "breeze-dark";
      };

      package = lib.mkOption {
        type = lib.types.package;
        default = pkgs.libsForQt5.breeze-icons;
      };
    };

    cursor = {
      name = lib.mkOption {
        type = lib.types.str;
        default = "breeze_cursors";
      };

      size = lib.mkOption {
        type = lib.types.int;
        default = builtins.floor (24 * scale);
      };

      package = lib.mkOption {
        type = lib.types.package;
        default = pkgs.libsForQt5.breeze-qt5;
      };
    };

    font = {
      general = {
        name = lib.mkOption {
          type = lib.types.str;
          default = "Noto Sans";
        };

        size = lib.mkOption {
          type = lib.types.int;
          default = 9;
        };

        package = lib.mkOption {
          type = lib.types.package;
          default = pkgs.noto-fonts;
        };
      };

      fixed = {
        name = lib.mkOption {
          type = lib.types.str;
          default = "Noto Sans Mono";
        };

        size = lib.mkOption {
          type = lib.types.int;
          default = 9;
        };

        package = lib.mkOption {
          type = lib.types.package;
          default = pkgs.noto-fonts;
        };
      };
    };

    color-scheme = lib.mkOption {
      type = lib.types.attrs;
      default = specialArgs.color-schemes.material-dark-kde;
      description = "Color scheme for polybar and other apps.";
    };
  };

  config = lib.mkIf enable {
    home.packages = [
      qt.package
      gtk.package
      cursor.package
      icon.package
    ];

    qt = {
      enable = true;
      platformTheme = "kde";
      style = {
        name = "breeze";
        package = qt.package;
      };
    };

    gtk = {
      enable = true;
      font = { inherit (font.general) name package size; };
      iconTheme = { inherit (icon) name package; };
      cursorTheme = { inherit (cursor) name package size; };
      theme = { inherit (gtk) name package; };
    };

    home.pointerCursor = rec {
      inherit (cursor) name package size;
      gtk.enable = true;
      x11.enable = true;
    };

    xdg.configFile = {
      # breezerc.text = toINI {
      #   Style.TabBarDrawCenteredTabs = true;
      # };

      # yes, kde nowdays just embeds the whole colorscheme to the config
      kdeglobals.text = toINI {
        General = {
          Name = "Breeze Dark";
          ColorScheme = "BreezeDark";
          XftHintStyle = "hintslight";
          XftSubPixel = "rgb";
          fixed = "Noto Sans Mono,10,-1,5,50,0,0,0,0,0";
          shadeSortColumn = true;
          smallestReadableFont = "Noto Sans,8,-1,5,50,0,0,0,0,0";
        };
        Icons.Theme = "breeze-dark";
        KDE = {
          contrast = 4;
          LookAndFeelPackage = "org.kde.breezedark.desktop";
        };
        WM = {
          activeBackground = "49,54,59";
          activeBlend = "252,252,252";
          activeForeground = "252,252,252";
          inactiveBackground = "42,46,50";
          inactiveBlend = "161,169,177";
          inactiveForeground = "161,169,177";
        };
        "ColorEffects:Disabled" = {
          Color = "56,56,56";
          ColorAmount = "0";
          ColorEffect = "0";
          ContrastAmount = "0.65";
          ContrastEffect = "1";
          IntensityAmount = "0.1";
          IntensityEffect = "2";
        };
        "ColorEffects:Inactive" = {
          ChangeSelectionColor = "true";
          Color = "112,111,110";
          ColorAmount = "0.025";
          ColorEffect = "2";
          ContrastAmount = "0.1";
          ContrastEffect = "2";
          Enable = "false";
          IntensityAmount = "0";
          IntensityEffect = "0";

        };
        "Colors:Button" = {
          BackgroundAlternate = "30,87,116";
          BackgroundNormal = "49,54,59";
          DecorationFocus = "61,174,233";
          DecorationHover = "61,174,233";
          ForegroundActive = "61,174,233";
          ForegroundInactive = "161,169,177";
          ForegroundLink = "29,153,243";
          ForegroundNegative = "218,68,83";
          ForegroundNeutral = "246,116,0";
          ForegroundNormal = "252,252,252";
          ForegroundPositive = "39,174,96";
          ForegroundVisited = "155,89,182";

        };
        "Colors:Complementary" = {
          BackgroundAlternate = "30,87,116";
          BackgroundNormal = "42,46,50";
          DecorationFocus = "61,174,233";
          DecorationHover = "61,174,233";
          ForegroundActive = "61,174,233";
          ForegroundInactive = "161,169,177";
          ForegroundLink = "29,153,243";
          ForegroundNegative = "218,68,83";
          ForegroundNeutral = "246,116,0";
          ForegroundNormal = "252,252,252";
          ForegroundPositive = "39,174,96";
          ForegroundVisited = "155,89,182";

        };
        "Colors:Header" = {
          BackgroundAlternate = "42,46,50";
          BackgroundNormal = "49,54,59";
          DecorationFocus = "61,174,233";
          DecorationHover = "61,174,233";
          ForegroundActive = "61,174,233";
          ForegroundInactive = "161,169,177";
          ForegroundLink = "29,153,243";
          ForegroundNegative = "218,68,83";
          ForegroundNeutral = "246,116,0";
          ForegroundNormal = "252,252,252";
          ForegroundPositive = "39,174,96";
          ForegroundVisited = "155,89,182";

        };
        "Colors:Header][Inactive" = {
          BackgroundAlternate = "49,54,59";
          BackgroundNormal = "42,46,50";
          DecorationFocus = "61,174,233";
          DecorationHover = "61,174,233";
          ForegroundActive = "61,174,233";
          ForegroundInactive = "161,169,177";
          ForegroundLink = "29,153,243";
          ForegroundNegative = "218,68,83";
          ForegroundNeutral = "246,116,0";
          ForegroundNormal = "252,252,252";
          ForegroundPositive = "39,174,96";
          ForegroundVisited = "155,89,182";

        };
        "Colors:Selection" = {
          BackgroundAlternate = "30,87,116";
          BackgroundNormal = "61,174,233";
          DecorationFocus = "61,174,233";
          DecorationHover = "61,174,233";
          ForegroundActive = "252,252,252";
          ForegroundInactive = "161,169,177";
          ForegroundLink = "253,188,75";
          ForegroundNegative = "176,55,69";
          ForegroundNeutral = "198,92,0";
          ForegroundNormal = "252,252,252";
          ForegroundPositive = "23,104,57";
          ForegroundVisited = "155,89,182";

        };
        "Colors:Tooltip" = {
          BackgroundAlternate = "42,46,50";
          BackgroundNormal = "49,54,59";
          DecorationFocus = "61,174,233";
          DecorationHover = "61,174,233";
          ForegroundActive = "61,174,233";
          ForegroundInactive = "161,169,177";
          ForegroundLink = "29,153,243";
          ForegroundNegative = "218,68,83";
          ForegroundNeutral = "246,116,0";
          ForegroundNormal = "252,252,252";
          ForegroundPositive = "39,174,96";
          ForegroundVisited = "155,89,182";

        };
        "Colors:View" = {
          BackgroundAlternate = "35,38,41";
          BackgroundNormal = "27,30,32";
          DecorationFocus = "61,174,233";
          DecorationHover = "61,174,233";
          ForegroundActive = "61,174,233";
          ForegroundInactive = "161,169,177";
          ForegroundLink = "29,153,243";
          ForegroundNegative = "218,68,83";
          ForegroundNeutral = "246,116,0";
          ForegroundNormal = "252,252,252";
          ForegroundPositive = "39,174,96";
          ForegroundVisited = "155,89,182";

        };
        "Colors:Window" = {
          BackgroundAlternate = "49,54,59";
          BackgroundNormal = "42,46,50";
          DecorationFocus = "61,174,233";
          DecorationHover = "61,174,233";
          ForegroundActive = "61,174,233";
          ForegroundInactive = "161,169,177";
          ForegroundLink = "29,153,243";
          ForegroundNegative = "218,68,83";
          ForegroundNeutral = "246,116,0";
          ForegroundNormal = "252,252,252";
          ForegroundPositive = "39,174,96";
        };
      };
    };
  };
}
