{ config, lib, pkgs, specialArgs, ... }:
let

  toINI = lib.generators.toINI { };

  # Other decent kvantum themes: 
  # https://github.com/EliverLara/Nordic
  # https://github.com/PapirusDevelopmentTeam/arc-kde
  # https://github.com/vinceliuice/WhiteSur-kde
  # https://github.com/PapirusDevelopmentTeam/materia-kde
  # https://github.com/catppuccin/Kvantum
  # https://github.com/vinceliuice/Colloid-kde
  # https://github.com/HimDek/Utterly-Nord-Plasma
  # https://github.com/EliverLara/Andromeda-KDE
  # https://github.com/HimDek/Utterly-Sweet-Plasma
  # https://github.com/HimDek/Utterly-Round-Plasma-Style
  package = pkgs.nordic;

  qtct = pkg:
    let
      name = builtins.baseNameOf (lib.getExe pkg);
    in
    toINI {
      Appearance = {
        color_scheme_path = "${pkg}/share/${name}/colors/airy.conf";
        custom_palette = false;
        icon_theme = "Nordic-darker";
        standard_dialogs = "xdgdesktopportal";
        style = "kvantum-dark";
      };

      Fonts = {
        fixed = "\"Noto Sans Mono,9,-1,5,50,0,0,0,0,0,Regular\"";
        general = "\"Noto Sans,9,-1,5,50,0,0,0,0,0,Regular\"";
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
  gtk = {
    enable = true;
    theme = {
      inherit package;
      name = "Nordic-darker";
    };
    iconTheme = {
      inherit package;
      name = "Nordic-darker";
    };
  };

  qt = {
    enable = true;
    platformTheme = "qtct";
    style = {
      name = "kvantum";
      package = [
        pkgs.libsForQt5.qtstyleplugin-kvantum
        pkgs.qt6Packages.qtstyleplugin-kvantum
      ];
    };
  };

  home.pointerCursor = rec {
    inherit package;
    name = "Nordic-cursors";
    size = 24;
    gtk.enable = true;
    x11 = {
      enable = true;
      defaultCursor = "Nordic-cursors";
    };
  };

  home.packages = [ package ];

  xdg = {
    enable = true;
    configFile = {
      "Kvantum/kvantum.kvconfig".text = toINI {
        General.theme = "Nordic-Darker";
      };
      "qt5ct/qt5ct.conf".text = qtct pkgs.qt5ct;
      "qt6ct/qt6ct.conf".text = qtct pkgs.qt6ct;
    };
  };
}

