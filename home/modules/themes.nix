# do not migrate to breeze, all qt5 apps will use gpu accel if you do
{ config, lib, pkgs, specialArgs, ... }:
let
  inherit (config.presets) archlinux;
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

  qt5ct =
    if archlinux then "/usr/share/qt5ct"
    else "${pkgs.qt5ct}/share/${qt5ct}";

  qt6ct =
    if archlinux then "/usr/share/qt6ct"
    else "${pkgs.qt6ct}/share/${qt6ct}";

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
        type = lib.types.package;
        default = pkgs.materia-kde-theme;
      };
    };

    gtk = {
      name = lib.mkOption {
        type = lib.types.str;
        default = "Materia-dark-compact";
      };

      package = lib.mkOption {
        type = lib.types.package;
        default = pkgs.materia-theme;
      };
    };

    icon = {
      name = lib.mkOption {
        type = lib.types.str;
        default = "Papirus-Dark";
      };

      package = lib.mkOption {
        type = lib.types.package;
        default = pkgs.papirus-icon-theme;
      };
    };

    cursor = {
      name = lib.mkOption {
        type = lib.types.str;
        default = "Simp1e-Mix-Dark"; # breeze_cursors
      };

      size = lib.mkOption {
        type = lib.types.int;
        default = builtins.floor (24 * scale);
      };

      package = lib.mkOption {
        type = lib.types.package;
        default = pkgs.simp1e-cursors.overrideAttrs (prev: rec {
          version = "20230817";
          src = pkgs.fetchFromGitLab {
            owner = "cursors";
            repo = "simp1e";
            rev = "3de6aa81683311bfca85e97cff520b3729ebc42a";
            sha256 = "sha256-p8+3LbPQ1siqSfyxMBEOiB0pR7x+c8/nAwZxZAN5sXU=";
            fetchSubmodules = true;
          };
        }); # pkgs.libsForQt5.breeze-qt5
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
      default = specialArgs.color-schemes.tomorrow-night-bright;
      description = "Color scheme for terminal.";
    };
  };

  config = lib.mkIf enable (lib.mkMerge [
    {
      home.packages = if archlinux then [ cursor.package ] else [
        qt.package
        gtk.package
        icon.package
        pkgs.libsForQt5.qt5ct
        pkgs.qt6Packages.qt6ct
        pkgs.libsForQt5.qtstyleplugin-kvantum
        pkgs.qt6Packages.qtstyleplugin-kvantum
      ];

      pacman.packages = [
        "extra/kvantum"
        "extra/qt5ct"
        "extra/qt6ct"
      ];

      home.sessionVariables = {
        QT_QPA_PLATFORMTHEME = "qt5ct";
        QT_STYLE_OVERRIDE = "qt5ct-style";
        QT_PLUGIN_PATH =
          if archlinux then "/usr/lib/qt/plugins/:/usr/lib/qt6/plugins"
          else "$QT_PLUGIN_PATH\${QT_PLUGIN_PATH:+:}" + (makeQtPath "qtPluginPrefix");
      };

      modules.xorg.imported-variables = [
        "QT_QPA_PLATFORMTHEME"
        "QT_STYLE_OVERRIDE"
        "QT_PLUGIN_PATH"
        "QML2_IMPORT_PATH"
      ];

      gtk = {
        enable = true;
        font = { inherit (font.general) name size; };
        iconTheme = { inherit (icon) name; };
        cursorTheme = { inherit (cursor) name package size; };
        theme = { inherit (gtk) name; };
      };

      home.pointerCursor = rec {
        inherit (cursor) name package size;
        gtk.enable = true;
        x11.enable = true;
      };

      xdg.configFile = lib.mkMerge [
        {
          "Kvantum/kvantum.kvconfig".text = toINI {
            General.theme = qt.kvantum-theme;
          };
          "qt5ct/qt5ct.conf".text = toQtct qt5ct;
          "qt6ct/qt6ct.conf".text = toQtct qt6ct;
        }
        (lib.mkIf (archlinux) {
          "Kvantum/${qt.kvantum-theme}".source = "${qt.package}/share/Kvantum/${qt.kvantum-theme}";
        })
      ];
    }
    (lib.mkIf (!archlinux) {
      # moonlight fix
      home.sessionVariables.QML2_IMPORT_PATH = "$QML2_IMPORT_PATH\${QML2_IMPORT_PATH:+:}" + (makeQtPath "qtQmlPrefix");

      gtk = {
        font.package = font.general.package;
        iconTheme.package = icon.package;
        theme.package = gtk.package;
      };
    })
  ]);
}
