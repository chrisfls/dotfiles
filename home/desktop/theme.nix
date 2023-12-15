{ config, lib, pkgs, ... }:
let
  cfg = config.extra;
  toINI = lib.generators.toINI { };
  toQtct = pkg:
    let
      name = builtins.baseNameOf (lib.getExe pkg);
    in
    toINI {
      Appearance = {
        color_scheme_path = "${pkg}/share/${name}/colors/airy.conf";
        custom_palette = false;
        icon_theme = cfg.icon-theme.name;
        standard_dialogs = "xdgdesktopportal";
        style = cfg.qt-theme.style;
      };

      Fonts = with cfg.fonts; {
        general = "\"#${general}\"";
        fixed = "\"${fixed}\"";
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
        stylesheets = "${pkg}/share/${name}/qss/scrollbar-simple.qss, ${pkg}/share/${name}/qss/sliders-simple.qss, ${pkg}/share/${name}/qss/tooltip-simple.qss, ${pkg}/share/${name}/qss/traynotification-simple.qss";
        toolbutton_style = 2;
        underline_shortcut = 2;
        wheel_scroll_lines = 1;
      };

      Troubleshooting = {
        force_raster_widgets = 1;
        ignored_applications = "@Invalid()";
      };
    };
in
{
  options.extra = {
    qt-theme = {
      enable = lib.mkEnableOption "description";

      style = lib.mkOption {
        type = lib.types.str;
        default = "kvantum-dark";
      };

      kvantum-theme = lib.mkOption {
        type = lib.types.str;
        default = "KvArcDark";
        description = ''
          Other decent kvantum themes:

          - https://github.com/EliverLara/Nordic
          - https://github.com/PapirusDevelopmentTeam/arc-kde
          - https://github.com/vinceliuice/WhiteSur-kde
          - https://github.com/PapirusDevelopmentTeam/materia-kde
          - https://github.com/catppuccin/Kvantum
          - https://github.com/vinceliuice/Colloid-kde
          - https://github.com/HimDek/Utterly-Nord-Plasma
          - https://github.com/EliverLara/Andromeda-KDE
          - https://github.com/HimDek/Utterly-Sweet-Plasma
          - https://github.com/HimDek/Utterly-Round-Plasma-Style
        '';
      };

      package = lib.mkOption {
        type = lib.types.package;
        default = pkgs.arc-kde-theme;
      };
    };

    gtk-theme = {
      enable = lib.mkEnableOption "description";

      name = lib.mkOption {
        type = lib.types.str;
        default = "Arc-Darker";
      };

      package = lib.mkOption {
        type = lib.types.package;
        default = pkgs.arc-theme;
      };
    };

    icon-theme = {
      enable = lib.mkEnableOption "description";

      name = lib.mkOption {
        type = lib.types.str;
        default = "kora";
      };

      package = lib.mkOption {
        type = lib.types.package;
        default = pkgs.kora-icon-theme;
      };
    };

    cursor-theme = {
      enable = lib.mkEnableOption "description";

      name = lib.mkOption {
        type = lib.types.str;
        default = "breeze_cursors";
      };

      package = lib.mkOption {
        type = lib.types.package;
        default = pkgs.libsForQt5.breeze-qt5;
      };
    };

    fonts = {
      general = lib.mkOption {
        type = lib.types.str;
        default = "Noto Sans,9,-1,5,50,0,0,0,0,0,Regular";
      };
      fixed = lib.mkOption {
        type = lib.types.str;
        default = "Noto Sans Mono,9,-1,5,50,0,0,0,0,0,Regular";
      };
    };
  };

  config = lib.mkMerge [
    (with cfg.qt-theme; lib.mkIf enable {
      qt = {
        enable = true;
        platformTheme = "qtct";
        style = {
          name = "kvantum";
          # probably redundant
          package = [
            pkgs.libsForQt5.qtstyleplugin-kvantum
            pkgs.qt6Packages.qtstyleplugin-kvantum
          ];
        };
      };

      xdg = {
        enable = true;
        configFile = {
          "Kvantum/kvantum.kvconfig".text = toINI {
            General.theme = kvantum-theme;
          };
          "qt5ct/qt5ct.conf".text = toQtct pkgs.qt5ct;
          "qt6ct/qt6ct.conf".text = toQtct pkgs.qt6ct;
        };
      };
    })

    (with cfg.gtk-theme; lib.mkIf enable
      {
        gtk = {
          enable = true;
          theme = { inherit name package; };
        };
      })

    (with cfg.icon-theme; lib.mkIf enable {
      gtk = { iconTheme = { inherit name package; }; };
    })

    (with cfg.cursor-theme; lib.mkIf enable {
      home.pointerCursor = {
        inherit name package;
        size = 24;
        gtk.enable = true;
        x11 = { enable = true; defaultCursor = name; };
      };
    })
  ];
}


