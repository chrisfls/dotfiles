{ config, lib, pkgs, specialArgs, ... }:
let
  color-scheme = specialArgs.color-schemes.popping-and-locking-black;
  qt-theme = pkgs.libsForQt5.breeze-qt5;
  gtk-theme = pkgs.libsForQt5.breeze-gtk;
  icon-theme = pkgs.libsForQt5.breeze-icons;
  toINI = lib.generators.toINI { };
in
{
  gtk = {
    enable = true;
    theme = {
      name = "Breeze-Dark";
      package = gtk-theme;
    };
    iconTheme = {
      name = "breeze-dark";
      package = icon-theme;
    };
  };

  qt = {
    enable = true;
    platformTheme = "kde";
    style = {
      name = "breeze";
      package = qt-theme;
    };
  };

  home.pointerCursor = rec {
    name = "breeze_cursors";
    package = qt-theme;
    size = 24;
    gtk.enable = true;
    x11 = {
      enable = true;
      defaultCursor = name;
    };
  };

  xdg = {
    enable = true;
    userDirs = {
      enable = true;
      createDirectories = true;
    };
    configFile = {
      breezerc.text = toINI {
        Style.TabBarDrawCenteredTabs = true;
      };

      # TODO: check if it is required
      "Trolltech.conf".text = toINI {
        qt = {
          "GUIEffects" = "none";
          "KDE\\contrast" = "7";
          "Palette\\active" = "#fcfcfc, #31363b, #474d54, #3a4045, #141618, #24282b, #fcfcfc, #ffffff, #fcfcfc, #1b1e20, #2a2e32, #0f1012, #3daee9, #fcfcfc, #1d99f3, #9b59b6, #232629, #000000, #31363b, #fcfcfc, #fcfcfc";
          "Palette\\disabled" = "#6e7173, #2f3338, #454c52, #393e44, #131517, #222629, #656768, #ffffff, #727679, #1a1d1f, #282c30, #0e0f11, #282c30, #6e7173, #1a4665, #443051, #212427, #000000, #31363b, #fcfcfc, #fcfcfc";
          "Palette\\inactive" = "#fcfcfc, #31363b, #474d54, #3a4045, #141618, #24282b, #fcfcfc, #ffffff, #fcfcfc, #1b1e20, #2a2e32, #0f1012, #1f485e, #fcfcfc, #1d99f3, #9b59b6, #232629, #000000, #31363b, #fcfcfc, #fcfcfc";
          "font" = "\"Noto Sans,10,-1,0,50,0,0,0,0,0\"";
        };
      };

      kdeglobals.text = toINI {
        General = {
          ColorScheme = "BreezeDark";
          XftHintStyle = "hintslight";
          XftSubPixel = "rgb";
          fixed = "Noto Sans Mono,10,-1,5,50,0,0,0,0,0";
          smallestReadableFont = "Noto Sans,8,-1,5,50,0,0,0,0,0";
          # Name = "Breeze Dark";
          # widgetStyle="breeze";
        };
        Icons.Theme = "breeze-dark";
        KDE.LookAndFeelPackage = "org.kde.breezedark.desktop";
        WM = {
          activeBackground = "49,54,59";
          activeBlend = "252,252,252";
          activeForeground = "252,252,252";
          inactiveBackground = "42,46,50";
          inactiveBlend = "161,169,177";
          inactiveForeground = "161,169,177";
        };
      };
    };
  };
}

/*
  [ColorEffects:Disabled]
  Color=56,56,56
  ColorAmount=0
  ColorEffect=0
  ContrastAmount=0.65
  ContrastEffect=1
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

*/
