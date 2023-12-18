{ config, lib, pkgs, specialArgs, ... }:
let
  cfg = config.extra.themes;

  scale = config.extra.scaling.scale;

  toINI = lib.generators.toINI { };

  fmtFont = { name, size, ... }:
    let
      # REVIEW:
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

  toQtct = pkg:
    let
      name = baseNameOf (lib.getExe pkg);
    in
    toINI {
      Appearance = {
        color_scheme_path = "${pkg}/share/${name}/colors/airy.conf";
        custom_palette = false;
        icon_theme = cfg.icon.name;
        standard_dialogs = "xdgdesktopportal";
        style = cfg.qt.style;
      };

      Fonts = {
        general = fmtFont cfg.font.general;
        fixed = fmtFont cfg.font.fixed;
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
  options.extra.themes = {
    enable = lib.mkEnableOption "Enable themes module";

    qt = {
      style = lib.mkOption {
        type = lib.types.str;
        default = "kvantum-dark";
      };

      # REVIEW: evaluate replacing ArcDark with materia kde
      kvantum-theme = lib.mkOption {
        type = lib.types.str;
        default = "ArcDark";
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

    gtk = {
      name = lib.mkOption {
        type = lib.types.str;
        default = "Arc-Dark";
      };

      package = lib.mkOption {
        type = lib.types.package;
        default = pkgs.arc-theme;
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
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ cfg.qt.package ];

    qt = {
      enable = true;
      platformTheme = "qtct";
      style.name = "kvantum";
    };

    gtk = {
      enable = true;
      font = { inherit (cfg.font.general) name package size; };
      iconTheme = { inherit (cfg.icon) name package; };
      theme = { inherit (cfg.gtk) name package; };
    };

    home.pointerCursor = rec {
      inherit (cfg.cursor) name package size;
      gtk.enable = true;
      x11 = { enable = true; defaultCursor = name; };
    };

    xdg.configFile = {
      "Kvantum/kvantum.kvconfig".text = toINI {
        General.theme = cfg.qt.kvantum-theme;
      };
      "qt5ct/qt5ct.conf".text = toQtct pkgs.qt5ct;
      "qt6ct/qt6ct.conf".text = toQtct pkgs.qt6ct;
    };
  };
}


