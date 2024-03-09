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
      "extra/qt5ct"
      "extra/qt6ct"
      cursor.package
      font.fixed.package
      font.general.package
      gtk.package
      icon.package
      qt5.package
      qt6.package
    ];

    home.sessionVariables = {
      QT_QPA_PLATFORMTHEME = "qt6ct";
      #QT_STYLE_OVERRIDE = "breeze";
    };

    modules.xorg.imported-variables = [
      "QT_QPA_PLATFORMTHEME"
      #"QT_STYLE_OVERRIDE"
    ];

    xresources.properties = {
      "Xcursor.theme" = cursor.name;
      "Xcursor.size" = cursor.size;
    };

    xdg.configFile = {
      "qt5ct/qt5ct.conf".text =
        ''
          [Appearance]
          color_scheme_path=/usr/share/qt5ct/colors/darker.conf
          custom_palette=true
          icon_theme=breeze-dark
          standard_dialogs=default
          style=Breeze

          [Fonts]
          fixed="Noto Sans,12,-1,5,50,0,0,0,0,0"
          general="Noto Sans,12,-1,5,50,0,0,0,0,0"

          [Interface]
          activate_item_on_single_click=0
          buttonbox_layout=0
          cursor_flash_time=1000
          dialog_buttons_have_icons=1
          double_click_interval=400
          gui_effects=General, AnimateMenu, AnimateCombo, AnimateTooltip, AnimateToolBox
          keyboard_scheme=0
          menus_have_icons=true
          show_shortcuts_in_context_menus=true
          stylesheets=/usr/share/qt5ct/qss/fusion-fixes.qss, /usr/share/qt5ct/qss/scrollbar-simple.qss, /usr/share/qt5ct/qss/sliders-simple.qss, /usr/share/qt5ct/qss/tooltip-simple.qss, /usr/share/qt5ct/qss/traynotification-simple.qss
          toolbutton_style=4
          underline_shortcut=1
          wheel_scroll_lines=3

          [SettingsWindow]
          geometry=@ByteArray(\x1\xd9\xd0\xcb\0\x3\0\0\0\0\vx\0\0\0\x18\0\0\rx\0\0\x5\x9f\0\0\vy\0\0\0)\0\0\rw\0\0\x5\x9e\0\0\0\0\0\0\0\0\n\0\0\0\vy\0\0\0)\0\0\rw\0\0\x5\x9e)

          [Troubleshooting]
          force_raster_widgets=0
          ignored_applications=@Invalid()
        '';
      "qt5ct/colorscheme.conf".text =
        ''
        '';
      "qt6ct/qt6ct.conf".text =
        ''
          [Appearance]
          color_scheme_path=/usr/share/qt6ct/colors/darker.conf
          custom_palette=true
          icon_theme=breeze-dark
          standard_dialogs=default
          style=Breeze

          [Fonts]
          fixed="Noto Sans,12,-1,5,400,0,0,0,0,0,0,0,0,0,0,1"
          general="Noto Sans,12,-1,5,400,0,0,0,0,0,0,0,0,0,0,1"

          [Interface]
          activate_item_on_single_click=0
          buttonbox_layout=0
          cursor_flash_time=1000
          dialog_buttons_have_icons=1
          double_click_interval=400
          gui_effects=General, AnimateMenu, AnimateCombo, AnimateTooltip, AnimateToolBox
          keyboard_scheme=0
          menus_have_icons=true
          show_shortcuts_in_context_menus=true
          stylesheets=/usr/share/qt6ct/qss/fusion-fixes.qss, /usr/share/qt6ct/qss/scrollbar-simple.qss, /usr/share/qt6ct/qss/sliders-simple.qss, /usr/share/qt6ct/qss/tooltip-simple.qss, /usr/share/qt6ct/qss/traynotification-simple.qss
          toolbutton_style=4
          underline_shortcut=1
          wheel_scroll_lines=3

          [SettingsWindow]
          geometry=@ByteArray(\x1\xd9\xd0\xcb\0\x3\0\0\0\0\n\x97\0\0\0\x18\0\0\r\x17\0\0\x5\x9f\0\0\n\x98\0\0\0)\0\0\r\x16\0\0\x5\x9e\0\0\0\0\0\0\0\0\n\0\0\0\n\x98\0\0\0)\0\0\r\x16\0\0\x5\x9e)

          [Troubleshooting]
          force_raster_widgets=0
          ignored_applications=@Invalid()
        '';
      "qt6ct/colorscheme.conf".text =
        ''
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
