# do not migrate to breeze, all qt5 apps will use gpu accel if you do
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

  fmtFont = { name, size, ... }:
    let
      font = lib.strings.concatStringsSep "," [
        name
        (builtins.toString size)
        # TODO: parameterize the rest
        "-1"
        "5"
        "50"
        "0"
        "0"
        "0"
        "0"
        "0"
        "Regular"
      ];
    in
    "\"${font}\"";

  toQtct = path:
    toINI {
      Appearance = {
        color_scheme_path = "${path}/colors/airy.conf";
        custom_palette = false;
        icon_theme = icon.name;
        standard_dialogs = "xdgdesktopportal";
        style = qt.style;
      };

      Fonts = {
        general = fmtFont font.general;
        fixed = fmtFont font.fixed;
      };

      Interface = {
        activate_item_on_single_click = 0;
        buttonbox_layout = 0;
        cursor_flash_time = 1000;
        dialog_buttons_have_icons = 2;
        double_click_interval = 250;
        gui_effects = "General, FadeMenu, AnimateCombo, FadeTooltip, AnimateToolBox";
        keyboard_scheme = 2;
        menus_have_icons = true;
        show_shortcuts_in_context_menus = true;
        stylesheets = "${path}/qss/scrollbar-simple.qss, ${path}/qss/sliders-simple.qss, ${path}/qss/tooltip-simple.qss, ${path}/qss/traynotification-simple.qss";
        toolbutton_style = 2;
        underline_shortcut = 2;
        wheel_scroll_lines = 3;
      };

      Troubleshooting = {
        force_raster_widgets = 1;
        ignored_applications = "@Invalid()";
      };
    };

  qt5ct = "/usr/share/qt5ct";
  qt6ct = "/usr/share/qt6ct";

  qtVersions = with pkgs; [ qt5 qt6 ];

  makeQtPath = prefix:
    lib.concatStringsSep ":"
      (map (qt: "${config.home.profileDirectory}/${qt.qtbase.${prefix}}") qtVersions);
in
{
  options.modules.themes = {
    enable = lib.mkEnableOption "Enable themes module";

    qt = {
      style = lib.mkOption {
        type = lib.types.str;
        default = "kvantum-dark";
      };

      kvantum-theme = lib.mkOption {
        type = lib.types.str;
        default = "MateriaDark";
        description = ''
          Other decent kvantum themes (ordered by stars):

          - https://github.com/EliverLara/Nordic [★★★☆☆]
          - https://github.com/PapirusDevelopmentTeam/arc-kde [★★★★★]
          - https://github.com/vinceliuice/WhiteSur-kde [★★★★☆]
          - https://github.com/PapirusDevelopmentTeam/materia-kde [★★★★★]
          - https://github.com/catppuccin/Kvantum [?????]
          - https://github.com/vinceliuice/Colloid-kde [★★★★☆]
          - https://github.com/HimDek/Utterly-Nord-Plasma [★★☆☆☆]
          - https://github.com/EliverLara/Andromeda-KDE (no pkg)
          - https://github.com/HimDek/Utterly-Sweet-Plasma [★★☆☆☆]
          - https://github.com/HimDek/Utterly-Round-Plasma-Style [★★☆☆☆]
        '';
      };

      package = lib.mkOption {
        type = lib.types.str;
        default = "extra/kvantum-theme-materia";
      };
    };

    gtk = {
      name = lib.mkOption {
        type = lib.types.str;
        default = "Materia-dark-compact";
      };

      package = lib.mkOption {
        type = lib.types.str;
        default = "extra/materia-gtk-theme";
      };
    };

    icon = {
      name = lib.mkOption {
        type = lib.types.str;
        default = "Papirus-Dark";
      };

      package = lib.mkOption {
        type = lib.types.str;
        default = "extra/papirus-icon-theme";
      };
    };

    cursor = {
      # TODO: eval this theme: https://github.com/keeferrourke/capitaine-cursors/tree/master
      name = lib.mkOption {
        type = lib.types.str;
        default = "Vanilla-DMZ-AA";
      };

      size = lib.mkOption {
        type = lib.types.int;
        default = builtins.floor (24 * scale);
      };

      package = lib.mkOption {
        type = lib.types.str;
        default = "extra/xcursor-vanilla-dmz-aa";
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
      "extra/kvantum"
      "extra/qt5ct"
      "extra/qt6ct"
      cursor.package
      font.fixed.package
      font.general.package
      gtk.package
      icon.package
      qt.package
    ];

    home.sessionVariables = {
      QT_QPA_PLATFORMTHEME = "qt5ct";
      QT_STYLE_OVERRIDE = "qt5ct-style";
      QT_PLUGIN_PATH = "/usr/lib/qt/plugins/:/usr/lib/qt6/plugins";
    };

    modules.xorg.imported-variables = [
      "QT_QPA_PLATFORMTHEME"
      "QT_STYLE_OVERRIDE"
      "QT_PLUGIN_PATH"
      "QML2_IMPORT_PATH"
    ];

    xresources.properties = {
      "Xcursor.theme" = cursor.name;
      "Xcursor.size" = cursor.size;
    };

    xdg.configFile = {
      "Kvantum/kvantum.kvconfig".text = toINI {
        General.theme = qt.kvantum-theme;
      };
      "qt5ct/qt5ct.conf".text = toQtct qt5ct;
      "qt6ct/qt6ct.conf".text = toQtct qt6ct;
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
