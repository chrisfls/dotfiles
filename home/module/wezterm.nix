{ config, lib, pkgs, specialArgs, ... }:
let
  cfg = config.module.wezterm;

  colors = config.module.themes.color-scheme;

  wezterm = "${config.programs.wezterm.package}/bin/wezterm";

  # launch =
  #   # LATER: https://wezfurlong.org/wezterm/config/lua/config/default_cwd.html
  #   ''
  #     dir=$(${pkgs.xcwd}/bin/xcwd);
  #     if [ "$dir" = "${config.home.homeDirectory}" ]; then
  #       ${wezterm} start;
  #     else
  #       ${wezterm} start --cwd "$dir";
  #     fi;
  #   '';

  launch = "exec \"$SCRIPT/wezterm\"";
in
{
  options.module.wezterm.enable = lib.mkEnableOption "Enable wezterm module";

  config = lib.mkIf cfg.enable {
    # module.sxhkd.keybindings = {
    #   "super + semicolon" = launch;
    #   "super + BackSpace" = launch;
    # };

    xsession.windowManager.i3.config.terminal = launch;

    module.script.install.wezterm =
      ''
        dir=$(${pkgs.xcwd}/bin/xcwd);
        if [ "$dir" = "${config.home.homeDirectory}" ]; then
          ${wezterm} start;
        else
          ${wezterm} start --cwd "$dir";
        fi;
      '';

    programs.wezterm = {
      enable = true;
      colorSchemes = {
        default = {
          ansi = [
            colors.black
            colors.red
            colors.green
            colors.yellow
            colors.blue
            colors.magenta
            colors.cyan
            colors.white
          ];
          brights = [
            colors.blackBright
            colors.redBright
            colors.greenBright
            colors.yellowBright
            colors.blueBright
            colors.magentaBright
            colors.cyanBright
            colors.whiteBright
          ];
          background = colors.background;
          foreground = colors.foreground;
          cursor_fg = colors.foreground;
          cursor_bg = colors.foreground;
          cursor_border = colors.foreground;
          selection_fg = colors.background;
          selection_bg = colors.foreground;
          scrollbar_thumb = colors.black;
        };
      };
      extraConfig =
        ''
          local wezterm = require 'wezterm'

          local config = wezterm.config_builder()

          config.default_prog = { '${pkgs.fish}/bin/fish' }
          config.default_cwd = "${config.xdg.userDirs.desktop}"

          config.color_scheme = 'default'

          config.font_size = 12.0
          config.font = wezterm.font {
            family = 'CaskaydiaCove NFM',
            harfbuzz_features = {
              'calt',
              'clig',
              'liga',
              'ss01', -- cursive italics
              'ss02', -- pretty ~=
              'ss19', -- slashed zero
              'ss20', -- graphical interface characters
            },
          }

          config.default_cursor_style = 'SteadyBar'
          config.cursor_thickness = 2

          config.window_background_opacity = 0.9

          config.window_padding = {
            left = '0.5cell',
            right = '0.5cell',
            top = '0.25cell',
            bottom = '0.25cell',
          }

          config.window_decorations = 'RESIZE'
          config.initial_rows = 24
          config.initial_cols = 80

          config.enable_scroll_bar = true
          config.scrollback_lines = 100000
          config.alternate_buffer_wheel_scroll_speed = 3; -- default

          config.enable_tab_bar = false
          config.mouse_wheel_scrolls_tabs = false

          local act = wezterm.action

          config.disable_default_key_bindings = true

          config.keys = {
            -- extra bindings
            { key = 'Backspace', mods = 'SUPER', action = act.SpawnWindow },
            { key = ';', mods = 'SUPER', action = act.SpawnWindow },

            --[[ -- disabled defaults
            { key = 'Tab', mods = 'CTRL', action = act.DisableDefaultAssignment },
            { key = 'Tab', mods = 'SHIFT|CTRL', action = act.DisableDefaultAssignment },
            { key = '!', mods = 'CTRL', action = act.DisableDefaultAssignment },
            { key = '!', mods = 'SHIFT|CTRL', action = act.DisableDefaultAssignment },
            { key = '\"', mods = 'ALT|CTRL', action = act.DisableDefaultAssignment },
            { key = '\"', mods = 'SHIFT|ALT|CTRL', action = act.DisableDefaultAssignment },
            { key = '#', mods = 'CTRL', action = act.DisableDefaultAssignment },
            { key = '#', mods = 'SHIFT|CTRL', action = act.DisableDefaultAssignment },
            { key = '$', mods = 'CTRL', action = act.DisableDefaultAssignment },
            { key = '$', mods = 'SHIFT|CTRL', action = act.DisableDefaultAssignment },
            { key = '%', mods = 'CTRL', action = act.DisableDefaultAssignment },
            { key = '%', mods = 'SHIFT|CTRL', action = act.DisableDefaultAssignment },
            { key = '%', mods = 'ALT|CTRL', action = act.DisableDefaultAssignment },
            { key = '%', mods = 'SHIFT|ALT|CTRL', action = act.DisableDefaultAssignment },
            { key = '&', mods = 'CTRL', action = act.DisableDefaultAssignment },
            { key = '&', mods = 'SHIFT|CTRL', action = act.DisableDefaultAssignment },
            { key = '${"\\\'"}', mods = 'SHIFT|ALT|CTRL', action = act.DisableDefaultAssignment },
            { key = '(', mods = 'CTRL', action = act.DisableDefaultAssignment },
            { key = '(', mods = 'SHIFT|CTRL', action = act.DisableDefaultAssignment },
            { key = '*', mods = 'CTRL', action = act.DisableDefaultAssignment },
            { key = '*', mods = 'SHIFT|CTRL', action = act.DisableDefaultAssignment },
            { key = '1', mods = 'SHIFT|CTRL', action = act.DisableDefaultAssignment },
            { key = '1', mods = 'SUPER', action = act.DisableDefaultAssignment },
            { key = '2', mods = 'SHIFT|CTRL', action = act.DisableDefaultAssignment },
            { key = '2', mods = 'SUPER', action = act.DisableDefaultAssignment },
            { key = '3', mods = 'SHIFT|CTRL', action = act.DisableDefaultAssignment },
            { key = '3', mods = 'SUPER', action = act.DisableDefaultAssignment },
            { key = '4', mods = 'SHIFT|CTRL', action = act.DisableDefaultAssignment },
            { key = '4', mods = 'SUPER', action = act.DisableDefaultAssignment },
            { key = '5', mods = 'SHIFT|CTRL', action = act.DisableDefaultAssignment },
            { key = '5', mods = 'SHIFT|ALT|CTRL', action = act.DisableDefaultAssignment },
            { key = '5', mods = 'SUPER', action = act.DisableDefaultAssignment },
            { key = '6', mods = 'SHIFT|CTRL', action = act.DisableDefaultAssignment },
            { key = '6', mods = 'SUPER', action = act.DisableDefaultAssignment },
            { key = '7', mods = 'SHIFT|CTRL', action = act.DisableDefaultAssignment },
            { key = '7', mods = 'SUPER', action = act.DisableDefaultAssignment },
            { key = '8', mods = 'SHIFT|CTRL', action = act.DisableDefaultAssignment },
            { key = '8', mods = 'SUPER', action = act.DisableDefaultAssignment },
            { key = '9', mods = 'SHIFT|CTRL', action = act.DisableDefaultAssignment },
            { key = '9', mods = 'SUPER', action = act.DisableDefaultAssignment },
            { key = '@', mods = 'CTRL', action = act.DisableDefaultAssignment },
            { key = '@', mods = 'SHIFT|CTRL', action = act.DisableDefaultAssignment },
            { key = 'N', mods = 'CTRL', action = act.DisableDefaultAssignment },
            { key = 'N', mods = 'SHIFT|CTRL', action = act.DisableDefaultAssignment },
            { key = 'T', mods = 'CTRL', action = act.DisableDefaultAssignment },
            { key = 'T', mods = 'SHIFT|CTRL', action = act.DisableDefaultAssignment },
            { key = '[', mods = 'SHIFT|SUPER', action = act.DisableDefaultAssignment },
            { key = ']', mods = 'SHIFT|SUPER', action = act.DisableDefaultAssignment },
            { key = '^', mods = 'CTRL', action = act.DisableDefaultAssignment },
            { key = '^', mods = 'SHIFT|CTRL', action = act.DisableDefaultAssignment },
            { key = 'n', mods = 'SHIFT|CTRL', action = act.DisableDefaultAssignment },
            { key = 'n', mods = 'SUPER', action = act.DisableDefaultAssignment },
            { key = 't', mods = 'SHIFT|CTRL', action = act.DisableDefaultAssignment },
            { key = 't', mods = 'SUPER', action = act.DisableDefaultAssignment },
            { key = '{', mods = 'SUPER', action = act.DisableDefaultAssignment },
            { key = '{', mods = 'SHIFT|SUPER', action = act.DisableDefaultAssignment },
            { key = '}', mods = 'SUPER', action = act.DisableDefaultAssignment },
            { key = '}', mods = 'SHIFT|SUPER', action = act.DisableDefaultAssignment },
            { key = 'PageUp', mods = 'CTRL', action = act.DisableDefaultAssignment },
            { key = 'PageUp', mods = 'SHIFT|CTRL', action = act.DisableDefaultAssignment },
            { key = 'PageDown', mods = 'CTRL', action = act.DisableDefaultAssignment },
            { key = 'PageDown', mods = 'SHIFT|CTRL', action = act.DisableDefaultAssignment },
            --]]

            -- mostly default bindings
            { key = 'Enter', mods = 'ALT', action = act.ToggleFullScreen },
            { key = ')', mods = 'CTRL', action = act.ResetFontSize },
            { key = ')', mods = 'SHIFT|CTRL', action = act.ResetFontSize },
            { key = '+', mods = 'CTRL', action = act.IncreaseFontSize },
            { key = '+', mods = 'SHIFT|CTRL', action = act.IncreaseFontSize },
            { key = '-', mods = 'CTRL', action = act.DecreaseFontSize },
            { key = '-', mods = 'SHIFT|CTRL', action = act.DecreaseFontSize },
            { key = '-', mods = 'SUPER', action = act.DecreaseFontSize },
            { key = '0', mods = 'CTRL', action = act.ResetFontSize },
            { key = '0', mods = 'SHIFT|CTRL', action = act.ResetFontSize },
            { key = '0', mods = 'SUPER', action = act.ResetFontSize },
            { key = '=', mods = 'CTRL', action = act.IncreaseFontSize },
            { key = '=', mods = 'SHIFT|CTRL', action = act.IncreaseFontSize },
            { key = '=', mods = 'SUPER', action = act.IncreaseFontSize },
            -- { key = 'C', mods = 'CTRL', action = act.CopyTo 'Clipboard' },
            { key = 'C', mods = 'SHIFT|CTRL', action = act.CopyTo 'Clipboard' },
            { key = 'F', mods = 'CTRL', action = act.Search 'CurrentSelectionOrEmptyString' },
            { key = 'F', mods = 'SHIFT|CTRL', action = act.Search 'CurrentSelectionOrEmptyString' },
            { key = 'K', mods = 'CTRL', action = act.ClearScrollback 'ScrollbackOnly' },
            { key = 'K', mods = 'SHIFT|CTRL', action = act.ClearScrollback 'ScrollbackOnly' },
            { key = 'L', mods = 'CTRL', action = act.ShowDebugOverlay },
            { key = 'L', mods = 'SHIFT|CTRL', action = act.ShowDebugOverlay },
            { key = 'M', mods = 'CTRL', action = act.Hide },
            { key = 'M', mods = 'SHIFT|CTRL', action = act.Hide },
            { key = 'P', mods = 'CTRL', action = act.ActivateCommandPalette },
            { key = 'P', mods = 'SHIFT|CTRL', action = act.ActivateCommandPalette },
            { key = 'R', mods = 'CTRL', action = act.ReloadConfiguration },
            { key = 'R', mods = 'SHIFT|CTRL', action = act.ReloadConfiguration },
            -- { key = 'U', mods = 'CTRL', action = act.CharSelect{ copy_on_select = true, copy_to =  'ClipboardAndPrimarySelection' } },
            -- { key = 'U', mods = 'SHIFT|CTRL', action = act.CharSelect{ copy_on_select = true, copy_to =  'ClipboardAndPrimarySelection' } },
            -- { key = 'V', mods = 'CTRL', action = act.PasteFrom 'Clipboard' },
            { key = 'V', mods = 'SHIFT|CTRL', action = act.PasteFrom 'Clipboard' },
            { key = 'W', mods = 'CTRL', action = act.CloseCurrentTab{ confirm = true } },
            { key = 'W', mods = 'SHIFT|CTRL', action = act.CloseCurrentTab{ confirm = true } },
            { key = 'X', mods = 'CTRL', action = act.ActivateCopyMode },
            { key = 'X', mods = 'SHIFT|CTRL', action = act.ActivateCopyMode },
            { key = 'Z', mods = 'CTRL', action = act.TogglePaneZoomState },
            { key = 'Z', mods = 'SHIFT|CTRL', action = act.TogglePaneZoomState },
            { key = '_', mods = 'CTRL', action = act.DecreaseFontSize },
            { key = '_', mods = 'SHIFT|CTRL', action = act.DecreaseFontSize },
            { key = 'c', mods = 'SHIFT|CTRL', action = act.CopyTo 'Clipboard' },
            -- { key = 'c', mods = 'SUPER', action = act.CopyTo 'Clipboard' },
            { key = 'f', mods = 'SHIFT|CTRL', action = act.Search 'CurrentSelectionOrEmptyString' },
            { key = 'f', mods = 'SUPER', action = act.Search 'CurrentSelectionOrEmptyString' },
            { key = 'k', mods = 'SHIFT|CTRL', action = act.ClearScrollback 'ScrollbackOnly' },
            { key = 'k', mods = 'SUPER', action = act.ClearScrollback 'ScrollbackOnly' },
            { key = 'l', mods = 'SHIFT|CTRL', action = act.ShowDebugOverlay },
            { key = 'm', mods = 'SHIFT|CTRL', action = act.Hide },
            { key = 'm', mods = 'SUPER', action = act.Hide },
            { key = 'p', mods = 'SHIFT|CTRL', action = act.ActivateCommandPalette },
            { key = 'r', mods = 'SHIFT|CTRL', action = act.ReloadConfiguration },
            { key = 'r', mods = 'SUPER', action = act.ReloadConfiguration },
            -- { key = 'u', mods = 'SHIFT|CTRL', action = act.CharSelect{ copy_on_select = true, copy_to =  'ClipboardAndPrimarySelection' } },
            { key = 'v', mods = 'SHIFT|CTRL', action = act.PasteFrom 'Clipboard' },
            -- { key = 'v', mods = 'SUPER', action = act.PasteFrom 'Clipboard' },
            { key = 'w', mods = 'SHIFT|CTRL', action = act.CloseCurrentTab{ confirm = true } },
            { key = 'w', mods = 'SUPER', action = act.CloseCurrentTab{ confirm = true } },
            { key = 'x', mods = 'SHIFT|CTRL', action = act.ActivateCopyMode },
            { key = 'z', mods = 'SHIFT|CTRL', action = act.TogglePaneZoomState },
            { key = 'phys:Space', mods = 'SHIFT|CTRL', action = act.QuickSelect },
            { key = 'PageUp', mods = 'SHIFT', action = act.ScrollByPage(-1) },
            { key = 'PageDown', mods = 'SHIFT', action = act.ScrollByPage(1) },
            { key = 'LeftArrow', mods = 'SHIFT|CTRL', action = act.ActivatePaneDirection 'Left' },
            { key = 'LeftArrow', mods = 'SHIFT|ALT|CTRL', action = act.AdjustPaneSize{ 'Left', 1 } },
            { key = 'RightArrow', mods = 'SHIFT|CTRL', action = act.ActivatePaneDirection 'Right' },
            { key = 'RightArrow', mods = 'SHIFT|ALT|CTRL', action = act.AdjustPaneSize{ 'Right', 1 } },
            { key = 'UpArrow', mods = 'SHIFT|CTRL', action = act.ActivatePaneDirection 'Up' },
            { key = 'UpArrow', mods = 'SHIFT|ALT|CTRL', action = act.AdjustPaneSize{ 'Up', 1 } },
            { key = 'DownArrow', mods = 'SHIFT|CTRL', action = act.ActivatePaneDirection 'Down' },
            { key = 'DownArrow', mods = 'SHIFT|ALT|CTRL', action = act.AdjustPaneSize{ 'Down', 1 } },
            { key = 'Insert', mods = 'SHIFT', action = act.PasteFrom 'PrimarySelection' },
            { key = 'Insert', mods = 'CTRL', action = act.CopyTo 'PrimarySelection' },
            { key = 'Copy', mods = 'NONE', action = act.CopyTo 'Clipboard' },
            { key = 'Paste', mods = 'NONE', action = act.PasteFrom 'Clipboard' },
          }

          -- also default bindings
          config.key_tables = {
            copy_mode = {
              { key = 'Tab', mods = 'NONE', action = act.CopyMode 'MoveForwardWord' },
              { key = 'Tab', mods = 'SHIFT', action = act.CopyMode 'MoveBackwardWord' },
              { key = 'Enter', mods = 'NONE', action = act.CopyMode 'MoveToStartOfNextLine' },
              { key = 'Escape', mods = 'NONE', action = act.CopyMode 'Close' },
              { key = 'Space', mods = 'NONE', action = act.CopyMode{ SetSelectionMode =  'Cell' } },
              { key = '$', mods = 'NONE', action = act.CopyMode 'MoveToEndOfLineContent' },
              { key = '$', mods = 'SHIFT', action = act.CopyMode 'MoveToEndOfLineContent' },
              { key = ',', mods = 'NONE', action = act.CopyMode 'JumpReverse' },
              { key = '0', mods = 'NONE', action = act.CopyMode 'MoveToStartOfLine' },
              { key = ';', mods = 'NONE', action = act.CopyMode 'JumpAgain' },
              { key = 'F', mods = 'NONE', action = act.CopyMode{ JumpBackward = { prev_char = false } } },
              { key = 'F', mods = 'SHIFT', action = act.CopyMode{ JumpBackward = { prev_char = false } } },
              { key = 'G', mods = 'NONE', action = act.CopyMode 'MoveToScrollbackBottom' },
              { key = 'G', mods = 'SHIFT', action = act.CopyMode 'MoveToScrollbackBottom' },
              { key = 'H', mods = 'NONE', action = act.CopyMode 'MoveToViewportTop' },
              { key = 'H', mods = 'SHIFT', action = act.CopyMode 'MoveToViewportTop' },
              { key = 'L', mods = 'NONE', action = act.CopyMode 'MoveToViewportBottom' },
              { key = 'L', mods = 'SHIFT', action = act.CopyMode 'MoveToViewportBottom' },
              { key = 'M', mods = 'NONE', action = act.CopyMode 'MoveToViewportMiddle' },
              { key = 'M', mods = 'SHIFT', action = act.CopyMode 'MoveToViewportMiddle' },
              { key = 'O', mods = 'NONE', action = act.CopyMode 'MoveToSelectionOtherEndHoriz' },
              { key = 'O', mods = 'SHIFT', action = act.CopyMode 'MoveToSelectionOtherEndHoriz' },
              { key = 'T', mods = 'NONE', action = act.CopyMode{ JumpBackward = { prev_char = true } } },
              { key = 'T', mods = 'SHIFT', action = act.CopyMode{ JumpBackward = { prev_char = true } } },
              { key = 'V', mods = 'NONE', action = act.CopyMode{ SetSelectionMode =  'Line' } },
              { key = 'V', mods = 'SHIFT', action = act.CopyMode{ SetSelectionMode =  'Line' } },
              { key = '^', mods = 'NONE', action = act.CopyMode 'MoveToStartOfLineContent' },
              { key = '^', mods = 'SHIFT', action = act.CopyMode 'MoveToStartOfLineContent' },
              { key = 'b', mods = 'NONE', action = act.CopyMode 'MoveBackwardWord' },
              { key = 'b', mods = 'ALT', action = act.CopyMode 'MoveBackwardWord' },
              { key = 'b', mods = 'CTRL', action = act.CopyMode 'PageUp' },
              { key = 'c', mods = 'CTRL', action = act.CopyMode 'Close' },
              { key = 'd', mods = 'CTRL', action = act.CopyMode{ MoveByPage = (0.5) } },
              { key = 'e', mods = 'NONE', action = act.CopyMode 'MoveForwardWordEnd' },
              { key = 'f', mods = 'NONE', action = act.CopyMode{ JumpForward = { prev_char = false } } },
              { key = 'f', mods = 'ALT', action = act.CopyMode 'MoveForwardWord' },
              { key = 'f', mods = 'CTRL', action = act.CopyMode 'PageDown' },
              { key = 'g', mods = 'NONE', action = act.CopyMode 'MoveToScrollbackTop' },
              { key = 'g', mods = 'CTRL', action = act.CopyMode 'Close' },
              { key = 'h', mods = 'NONE', action = act.CopyMode 'MoveLeft' },
              { key = 'j', mods = 'NONE', action = act.CopyMode 'MoveDown' },
              { key = 'k', mods = 'NONE', action = act.CopyMode 'MoveUp' },
              { key = 'l', mods = 'NONE', action = act.CopyMode 'MoveRight' },
              { key = 'm', mods = 'ALT', action = act.CopyMode 'MoveToStartOfLineContent' },
              { key = 'o', mods = 'NONE', action = act.CopyMode 'MoveToSelectionOtherEnd' },
              { key = 'q', mods = 'NONE', action = act.CopyMode 'Close' },
              { key = 't', mods = 'NONE', action = act.CopyMode{ JumpForward = { prev_char = true } } },
              { key = 'u', mods = 'CTRL', action = act.CopyMode{ MoveByPage = (-0.5) } },
              { key = 'v', mods = 'NONE', action = act.CopyMode{ SetSelectionMode =  'Cell' } },
              { key = 'v', mods = 'CTRL', action = act.CopyMode{ SetSelectionMode =  'Block' } },
              { key = 'w', mods = 'NONE', action = act.CopyMode 'MoveForwardWord' },
              { key = 'y', mods = 'NONE', action = act.Multiple{ { CopyTo =  'ClipboardAndPrimarySelection' }, { CopyMode =  'Close' } } },
              { key = 'PageUp', mods = 'NONE', action = act.CopyMode 'PageUp' },
              { key = 'PageDown', mods = 'NONE', action = act.CopyMode 'PageDown' },
              { key = 'End', mods = 'NONE', action = act.CopyMode 'MoveToEndOfLineContent' },
              { key = 'Home', mods = 'NONE', action = act.CopyMode 'MoveToStartOfLine' },
              { key = 'LeftArrow', mods = 'NONE', action = act.CopyMode 'MoveLeft' },
              { key = 'LeftArrow', mods = 'ALT', action = act.CopyMode 'MoveBackwardWord' },
              { key = 'RightArrow', mods = 'NONE', action = act.CopyMode 'MoveRight' },
              { key = 'RightArrow', mods = 'ALT', action = act.CopyMode 'MoveForwardWord' },
              { key = 'UpArrow', mods = 'NONE', action = act.CopyMode 'MoveUp' },
              { key = 'DownArrow', mods = 'NONE', action = act.CopyMode 'MoveDown' },
            },

            search_mode = {
              { key = 'Enter', mods = 'NONE', action = act.CopyMode 'PriorMatch' },
              { key = 'Escape', mods = 'NONE', action = act.CopyMode 'Close' },
              { key = 'n', mods = 'CTRL', action = act.CopyMode 'NextMatch' },
              { key = 'p', mods = 'CTRL', action = act.CopyMode 'PriorMatch' },
              { key = 'r', mods = 'CTRL', action = act.CopyMode 'CycleMatchType' },
              { key = 'u', mods = 'CTRL', action = act.CopyMode 'ClearPattern' },
              { key = 'PageUp', mods = 'NONE', action = act.CopyMode 'PriorMatchPage' },
              { key = 'PageDown', mods = 'NONE', action = act.CopyMode 'NextMatchPage' },
              { key = 'UpArrow', mods = 'NONE', action = act.CopyMode 'PriorMatch' },
              { key = 'DownArrow', mods = 'NONE', action = act.CopyMode 'NextMatch' },
            },
          }

          return config
        '';
    };
  };
}
