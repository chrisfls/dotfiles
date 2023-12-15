{ config, lib, pkgs, specialArgs, ... }:
let
  cfg = config.extra;
  toINI = lib.generators.toINI { };
  toQtct = pkg:
    let
      name = builtins.baseNameOf (lib.getExe pkg);
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
    in
    toINI {
      Appearance = {
        color_scheme_path = "${pkg}/share/${name}/colors/airy.conf";
        custom_palette = false;
        icon_theme = cfg.icon-theme.name;
        standard_dialogs = "xdgdesktopportal";
        style = cfg.qt-theme.style;
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
      enable = lib.mkEnableOption "Manage gtk themes";

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
      enable = lib.mkEnableOption "Manage icon themes";

      name = lib.mkOption {
        type = lib.types.str;
        default = "breeze-dark";
      };

      package = lib.mkOption {
        type = lib.types.package;
        default = pkgs.libsForQt5.breeze-icons;
      };
    };

    cursor-theme = {
      enable = lib.mkEnableOption "Manage cursor themes";

      name = lib.mkOption {
        type = lib.types.str;
        default = "Breeze_Snow";
      };

      size = lib.mkOption {
        type = lib.types.int;
        default = 24;
      };

      package = lib.mkOption {
        type = lib.types.package;
        default = pkgs.libsForQt5.breeze-qt5;
      };
    };

    font = {
      enable = lib.mkEnableOption "Manage fonts";

      general = {
        name = lib.mkOption {
          type = lib.types.str;
          default = "Overpass";
        };

        size = lib.mkOption {
          type = lib.types.int;
          default = 12;
        };

        package = lib.mkOption {
          type = lib.types.package;
          default = pkgs.overpass;
        };
      };

      fixed = {
        name = lib.mkOption {
          type = lib.types.str;
          default = "Overpass Mono";
        };

        size = lib.mkOption {
          type = lib.types.int;
          default = 12;
        };

        package = lib.mkOption {
          type = lib.types.package;
          default = pkgs.overpass;
        };
      };

      extra = lib.mkOption {
        type = lib.types.listOf lib.types.package;
        default = [
          pkgs.noto-fonts
          pkgs.jetbrains-mono
          pkgs.cascadia-code
          # pkgs.nerdfonts
        ];
      };
    };
  };

  config = lib.mkMerge [
    (lib.mkIf cfg.qt-theme.enable {
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
            General.theme = cfg.qt-theme.kvantum-theme;
          };
          "qt5ct/qt5ct.conf".text = toQtct pkgs.qt5ct;
          "qt6ct/qt6ct.conf".text = toQtct pkgs.qt6ct;
        };
      };
    })

    (lib.mkIf cfg.gtk-theme.enable {
      gtk = {
        enable = true;
        theme = { inherit (cfg.gtk-theme) name package; };
      };
    })

    (lib.mkIf cfg.icon-theme.enable {
      gtk = {
        iconTheme = { inherit (cfg.icon-theme) name package; };
      };
    })

    (lib.mkIf cfg.cursor-theme.enable {
      home.pointerCursor = rec {
        inherit (cfg.cursor-theme) name package size;
        gtk.enable = true;
        x11 = { enable = true; defaultCursor = name; };
      };
    })

    (lib.mkIf cfg.font.enable {
      home.packages = cfg.font.extra;
      extra.copyFile."$XDG_CONFIG_HOME/fontconfig/fonts.conf" = ./theme/fontconfig.conf;
      fonts.fontconfig.enable = true;
    })

    (lib.mkIf (cfg.gtk-theme.enable && cfg.font.enable) {
      gtk.font = { inherit (cfg.font.general) name package size; };
    })
  ];
}


