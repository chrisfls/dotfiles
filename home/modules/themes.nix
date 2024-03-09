{ config, lib, pkgs, specialArgs, ... }:
let
  inherit (config.modules.themes)
    cursor
    enable
    font
    gtk
    icon
    qt5
    qt6;

  inherit (config.modules.scaling) scale;
in
{
  options.modules.themes = {
    enable = lib.mkEnableOption "Enable themes module";

    qt5.package = lib.mkOption {
      type = lib.types.str;
      default = "extra/breeze5";
    };

    qt6.package = lib.mkOption {
      type = lib.types.str;
      default = "extra/breeze";
    };

    gtk = {
      name = lib.mkOption {
        type = lib.types.str;
        default = "Breeze-Dark";
      };

      package = lib.mkOption {
        type = lib.types.str;
        default = "extra/breeze-gtk";
      };
    };

    icon = {
      name = lib.mkOption {
        type = lib.types.str;
        default = "breeze-dark";
      };

      package = lib.mkOption {
        type = lib.types.str;
        default = "extra/breeze-icons";
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
        type = lib.types.str;
        default = "extra/breeze";
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
          type = lib.types.str;
          default = "extra/noto-fonts";
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
          type = lib.types.str;
          default = "extra/noto-fonts";
        };
      };
    };

    color-scheme = lib.mkOption {
      type = lib.types.attrs;
      default = specialArgs.color-schemes.tomorrow-night-bright;
      description = "Color scheme for terminal.";
    };
  };

  config = lib.mkIf enable {
    pacman.packages = [
      "extra/plasma-integration"
      "extra/plasma5-integration"
      "extra/systemsettings"
      cursor.package
      font.fixed.package
      font.general.package
      gtk.package
      icon.package
      qt5.package
      qt6.package
    ];

    home.sessionVariables = {
      QT_QPA_PLATFORMTHEME = "kde";
      QT_STYLE_OVERRIDE = "breeze";
    };

    modules.xorg.imported-variables = [
      "QT_QPA_PLATFORMTHEME"
      "QT_STYLE_OVERRIDE"
    ];

    xresources.properties = {
      "Xcursor.theme" = cursor.name;
      "Xcursor.size" = cursor.size;
    };

    xdg.configFile = {
      kdeglobals.text =
        ''
          [ColorEffects:Disabled]
          ChangeSelectionColor=
          Color=56,56,56
          ColorAmount=0
          ColorEffect=0
          ContrastAmount=0.65
          ContrastEffect=1
          Enable=
          IntensityAmount=0.1
          IntensityEffect=2

          [ColorEffects:Inactive]
          ChangeSelectionColor=true
          Color=112,111,110
          ColorAmount=0.025
          ColorEffect=2
          ContrastAmount=0.1
          ContrastEffect=2
          Enable=false
          IntensityAmount=0
          IntensityEffect=0

          [Colors:Button]
          BackgroundAlternate=30,87,116
          BackgroundNormal=49,54,59
          DecorationFocus=61,174,233
          DecorationHover=61,174,233
          ForegroundActive=61,174,233
          ForegroundInactive=161,169,177
          ForegroundLink=29,153,243
          ForegroundNegative=218,68,83
          ForegroundNeutral=246,116,0
          ForegroundNormal=252,252,252
          ForegroundPositive=39,174,96
          ForegroundVisited=155,89,182

          [Colors:Complementary]
          BackgroundAlternate=30,87,116
          BackgroundNormal=42,46,50
          DecorationFocus=61,174,233
          DecorationHover=61,174,233
          ForegroundActive=61,174,233
          ForegroundInactive=161,169,177
          ForegroundLink=29,153,243
          ForegroundNegative=218,68,83
          ForegroundNeutral=246,116,0
          ForegroundNormal=252,252,252
          ForegroundPositive=39,174,96
          ForegroundVisited=155,89,182

          [Colors:Header]
          BackgroundAlternate=42,46,50
          BackgroundNormal=49,54,59
          DecorationFocus=61,174,233
          DecorationHover=61,174,233
          ForegroundActive=61,174,233
          ForegroundInactive=161,169,177
          ForegroundLink=29,153,243
          ForegroundNegative=218,68,83
          ForegroundNeutral=246,116,0
          ForegroundNormal=252,252,252
          ForegroundPositive=39,174,96
          ForegroundVisited=155,89,182

          [Colors:Header][Inactive]
          BackgroundAlternate=49,54,59
          BackgroundNormal=42,46,50
          DecorationFocus=61,174,233
          DecorationHover=61,174,233
          ForegroundActive=61,174,233
          ForegroundInactive=161,169,177
          ForegroundLink=29,153,243
          ForegroundNegative=218,68,83
          ForegroundNeutral=246,116,0
          ForegroundNormal=252,252,252
          ForegroundPositive=39,174,96
          ForegroundVisited=155,89,182

          [Colors:Selection]
          BackgroundAlternate=30,87,116
          BackgroundNormal=61,174,233
          DecorationFocus=61,174,233
          DecorationHover=61,174,233
          ForegroundActive=252,252,252
          ForegroundInactive=161,169,177
          ForegroundLink=253,188,75
          ForegroundNegative=176,55,69
          ForegroundNeutral=198,92,0
          ForegroundNormal=252,252,252
          ForegroundPositive=23,104,57
          ForegroundVisited=155,89,182

          [Colors:Tooltip]
          BackgroundAlternate=42,46,50
          BackgroundNormal=49,54,59
          DecorationFocus=61,174,233
          DecorationHover=61,174,233
          ForegroundActive=61,174,233
          ForegroundInactive=161,169,177
          ForegroundLink=29,153,243
          ForegroundNegative=218,68,83
          ForegroundNeutral=246,116,0
          ForegroundNormal=252,252,252
          ForegroundPositive=39,174,96
          ForegroundVisited=155,89,182

          [Colors:View]
          BackgroundAlternate=35,38,41
          BackgroundNormal=27,30,32
          DecorationFocus=61,174,233
          DecorationHover=61,174,233
          ForegroundActive=61,174,233
          ForegroundInactive=161,169,177
          ForegroundLink=29,153,243
          ForegroundNegative=218,68,83
          ForegroundNeutral=246,116,0
          ForegroundNormal=252,252,252
          ForegroundPositive=39,174,96
          ForegroundVisited=155,89,182

          [Colors:Window]
          BackgroundAlternate=49,54,59
          BackgroundNormal=42,46,50
          DecorationFocus=61,174,233
          DecorationHover=61,174,233
          ForegroundActive=61,174,233
          ForegroundInactive=161,169,177
          ForegroundLink=29,153,243
          ForegroundNegative=218,68,83
          ForegroundNeutral=246,116,0
          ForegroundNormal=252,252,252
          ForegroundPositive=39,174,96
          ForegroundVisited=155,89,182

          [KDE]
          LookAndFeelPackage=org.kde.breezedark.desktop

          [WM]
          activeBackground=49,54,59
          activeBlend=252,252,252
          activeForeground=252,252,252
          inactiveBackground=42,46,50
          inactiveBlend=161,169,177
          inactiveForeground=161,169,177
        '';
      "gtk-3.0/settings.ini".text =
        ''
          [Settings]
          gtk-cursor-theme-name=${cursor.name}
          gtk-cursor-theme-size=${toString cursor.size}
          gtk-font-name=${font.general.name} ${toString font.general.size}
          gtk-icon-theme-name=${icon.name}
          gtk-theme-name=${gtk.name}
        '';
      "gtk-4.0/settings.ini".text =
        ''
          [Settings]
          gtk-cursor-theme-name=${cursor.name}
          gtk-cursor-theme-size=${toString cursor.size}
          gtk-font-name=${font.general.name} ${toString font.general.size}
          gtk-icon-theme-name=${icon.name}
          gtk-theme-name=${gtk.name}
        '';
    };
  };
}
