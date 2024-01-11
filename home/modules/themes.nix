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
        stylesheets = "${pkg}/share/${name}/qss/scrollbar-simple.qss, ${pkg}/share/${name}/qss/sliders-simple.qss, ${pkg}/share/${name}/qss/tooltip-simple.qss, ${pkg}/share/${name}/qss/traynotification-simple.qss";
        toolbutton_style = 2;
        underline_shortcut = 2;
        wheel_scroll_lines = 3;
      };

      Troubleshooting = {
        force_raster_widgets = 1;
        ignored_applications = "@Invalid()";
      };
    };
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
      default = specialArgs.color-schemes.material-dark-kde-alt;
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
      platformTheme = "qtct";
      style.name = "kvantum";
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
      "Kvantum/kvantum.kvconfig".text = toINI {
        General.theme = qt.kvantum-theme;
      };
      "qt5ct/qt5ct.conf".text = toQtct pkgs.qt5ct;
      "qt6ct/qt6ct.conf".text = toQtct pkgs.qt6ct;
    };
  };
}
