##
# Performance ranking using github.com/cmuratori/termbench
#
#  1st foot
#
#    parser:   fastest 
#    render:   fast cpu render
#    latency:  low
#    features: lots, but no ligatures 
#    protocol: wayland
#
#  2nd zutty
#
#    parser:   fast
#    render:   fast gpu render
#    latency:  low
#    features: few
#    protocol: both
#
#  3rd alacritty
#
#    parser:   fast (slower than foot)
#    render:   fast gpu render
#    latency:  mid
#    features: few
#    protocol: both + windows
#
#  4th contour
#
#    parser:   fast like alacritty
#    render:   fast gpu render
#    latency:  unknown
#    features: lots
#    protocol: both
#
#  5th kitty
#
#    parser:   slower than contour
#    render:   fast gpu render
#    latency:  unknown
#    features: lots
#    protocol: both + windows
#
#  6th wezterm
#
#    parser:   lot slower than kitty
#    render:   gpu render
#    latency:  unknown
#    features: most of all
#    protocol: both + windows
##
{ config, lib, pkgs, specialArgs, ... }:
let
  cfg = config.module.contour;

  scale = config.module.scaling.scale;

  theme = specialArgs.color-schemes.popping-and-locking-black;

  pkg = pkgs.contour;
in
{
  options.module.contour.enable = lib.mkEnableOption "Enable contour module";

  config = lib.mkIf cfg.enable {
    home.packages = [ pkg ];

    services.sxhkd.keybindings = {
      "super + semicolon" = "contour";
      "super + BackSpace" = "contour";
    };

    # Default Contour Configuration File.
    xdg.configFile."contour/contour.yml".text = lib.generators.toYAML { } {
      # Overrides the auto-detected platform plugin to be loaded.
      #
      # Possible (incomplete list of) values are:
      # - auto        The platform will be auto-detected.
      # - xcb         Uses XCB plugin (for X11 environment).
      # - cocoa       Used to be run on Mac OS/X.
      # - direct2d    Windows platform plugin using Direct2D.
      # - winrt       Windows platform plugin using WinRT.
      #
      # Default: auto
      platform_plugin = "auto";

      # VT Renderer configuration.
      # ADVANCED! Do not touch unless you know what you are doing.
      renderer = {
        # Backend to use for rendering the terminal onto the screen
        #
        # Possible values are:
        # - default     Uses the default rendering option as decided by the terminal.
        # - software    Uses software-based rendering.
        # - OpenGL      Use (possibly) hardware accelerated OpenGL
        backend = "OpenGL";

        # Number of hashtable slots to map to the texture tiles.
        # Larger values may increase performance, but too large may also decrease.
        # This value is rounted up to a value equal to the power of two.
        #
        # Default: 4096
        tile_hashtable_slots = 4096;

        # Number of tiles that must fit at lest into the texture atlas.
        #
        # This does not include direct mapped tiles (US-ASCII glyphs,
        # cursor shapes and decorations), if tile_direct_mapping is set to true).
        #
        # Value must be at least as large as grid cells available in the terminal view.
        # This value is automatically adjusted if too small.
        #
        # Default: 4000
        tile_cache_count = 4000;

        # Enables/disables the use of direct-mapped texture atlas tiles for
        # the most often used ones (US-ASCII, cursor shapes, underline styles)
        # You most likely do not wnat to touch this.
        #
        # Default: true
        tile_direct_mapping = true;
      };

      # Word delimiters when selecting word-wise.
      word_delimiters = " /\\()\"'-.,:;<>~!@#$%^&*+=[]{}~?|│";

      # Default PTY read buffer size.
      #
      # This is an advance option. Use with care!
      # Default: 16384
      read_buffer_size = 16384;

      # Size in bytes per PTY Buffer Object.
      #
      # This is an advanced option of an internal storage. Only change with care!
      pty_buffer_size = 1048576;
      default_profile = "main";

      # Flag to determine whether to spawn new process or not when creating new terminal
      # Default: false
      spawn_new_process = false;

      # Whether or not to reflow the lines on terminal resize events.
      # Default: true
      reflow_on_resize = true;

      # Section of experimental features.
      # All experimental features are disabled by default and must be explicitely enabled here.
      # NOTE: Contour currently has no experimental features behind this configuration wall.
      # experimental:
      #     # Enables experimental support for feature X/Y/Z
      #     feature_xyz: true

      # This keyboard modifier can be used to bypass the terminal's mouse protocol,
      # which can be used to select screen content even if the an application
      # mouse protocol has been activated (Default: Shift).
      #
      # The same modifier values apply as with input modifiers (see below).
      bypass_mouse_protocol_modifier = "Shift";

      # Modifier to be pressed in order to initiate block-selection
      # using the left mouse button.
      #
      # This is usually the Control modifier, but on OS/X that is not possible,
      # so Alt or Meta would be recommended instead.
      #
      # Supported modifiers:
      # - Alt
      # - Control
      # - Shift
      # - Meta
      #
      # Default: Control
      mouse_block_selection_modifier = "Control";

      # Selects an action to perform when a text selection has been made.
      #
      # Possible values are:
      #
      # - None                        Does nothing
      # - CopyToClipboard             Copies the selection to the primary clipboard.
      # - CopyToSelectionClipboard    Copies the selection to the selection clipboard.
      #                               This is not supported on all platforms.
      #
      # Default: CopyToSelectionClipboard
      on_mouse_select = "CopyToSelectionClipboard";

      # Inline image related default configuration and limits
      # -----------------------------------------------------
      images = {
        # Enable or disable sixel scrolling (SM/RM ?80 default)
        sixel_scrolling = true;

        # Configures the maximum number of color registers available when rendering Sixel graphics.
        sixel_register_count = 4096;

        # If enabled, the ANSI text cursor is placed at the position of the sixel graphics cursor after
        # image rendering, otherwise (if disabled) the cursor is placed underneath the image.
        sixel_cursor_conformance = true;

        # maximum width in pixels of an image to be accepted (0 defaults to system screen pixel width)
        max_width = 0;

        # maximum height in pixels of an image to be accepted (0 defaults to system screen pixel height)
        max_height = 0;
      };

      # Terminal Profiles
      # -----------------
      #
      # Dominates how your terminal visually looks like. You will need at least one terminal profile.
      profiles = {
        main = {
          # You can override the process to be started inside the terminal.
          # If nothing is specified, the users' default login shell will be used.
          # But you may as well log in to a remote host.
          # shell: "ssh ubuntu-vm"
          shell = "${pkgs.fish}/bin/fish";

          # arguments = ["some" "optional" "arguments" "for" "the" "shell"];

          # Advanced value that is useful when CopyPreviousMarkRange is used
          # with multiline-prompts. This offset value is being added to the
          # current cursor's line number minus 1 (i.e. the line above the current cursor).
          #
          # Default value is 0.
          copy_last_mark_range_offset = 0;

          # Sets initial working directory when spawning a new terminal.
          # A leading ~ is expanded to the user's home directory.
          # Default value is the user's home directory.
          initial_working_directory = "~/Desktop";

          # When this profile is *activated*, this flag decides
          # whether or not the title bar will be shown
          show_title_bar = false;

          # When this profile is being *activated*, this flag decides
          # whether or not to put the terminal's screen into fullscreen mode.
          #
          # It is activated during startup as well as when switching from another profile to this one.
          fullscreen = false;

          # When this profile is *activated*, this flag decides
          # whether or not to put the window into maximized mode.
          maximized = false;

          # Defines the class part of the WM_CLASS property of the window.
          wm_class = "contour";

          # Environment variables to be passed to the shell.
          # environment:
          #     TERM: contour
          #     COLORTERM: truecolor

          # Determines the terminal type that is being advertised.
          # Possible values are:
          #   - VT100
          #   - VT220
          #   - VT240
          #   - VT330
          #   - VT340
          #   - VT320
          #   - VT420
          #   - VT510
          #   - VT520
          #   - VT525
          # Default: VT525
          terminal_id = "VT525";

          # Determines the initial terminal sice in characters.
          terminal_size = {
            columns = 80;
            lines = 25;
          };

          history = {
            # Number of lines to preserve (-1 for infinite).
            limit = 1000000;

            # Boolean indicating whether or not to scroll down to the bottom on screen updates.
            auto_scroll_on_update = false;

            # Number of lines to scroll on ScrollUp & ScrollDown events.
            # Default: 3
            scroll_multiplier = 3;
          };

          # visual scrollbar support
          scrollbar = {
            # scroll bar position: Left, Right, Hidden (ignore-case)
            position = "right";

            # whether or not to hide the scrollbar when in alt-screen.
            hide_in_alt_screen = true;
          };

          # Some VT sequences should need access permissions.
          #
          # These can be to:
          # - allow     Allows the given functionality
          # - deny      Denies the given functionality
          # - ask       Asks the user interactively via popup dialog for permission of the given action.
          #
          # Default for all of these entries should be: "ask".
          permissions = {
            # Allows changing the font via `OSC 50 ; Pt ST`.
            change_font = "ask";

            # Allows capturing the screen buffer via `CSI > Pm ; Ps ; Pc ST`.
            # The response can be read from stdin as sequence `OSC 314 ; <screen capture> ST`
            capture_buffer = "ask";
          };

          # Font related configuration (font face, styles, size, rendering mode).
          font = {
            # Initial font size in pixels.
            size = 12;

            # DPI scaling factor applied on top of the system configured on (default: 1.0).
            dpi_scale = scale;

            # Font Locator API
            # Selects an engine to use for locating font files on the system.
            # This is implicitly also responsible for font fallback
            # Possible values are:
            # - fontconfig      : uses fontconfig to select fonts
            # - CoreText        : uses OS/X CoreText to select fonts (currently not implemented).
            # - DirectWrite     : selects DirectWrite engine (Windows only)
            locator = "fontconfig";

            # Text shaping related settings
            text_shaping = {
              # Selects which text shaping and font rendering engine to use.
              # Supported values are:
              # - native      : automatically choose the best available on the current platform.
              # - DirectWrite : selects DirectWrite engine (Windows only)
              # - CoreText    : selects CoreText engine (Mac OS/X only)
              # - OpenShaper  : selects OpenShaper (harfbuzz/freetype/fontconfig, avilable on all
              #                 platforms)
              engine = "OpenShaper";
            };

            # Uses builtin textures for pixel-perfect box drawing.
            # If disabled, the font's provided box drawing characters
            # will be used (Default: true).
            builtin_box_drawing = true;

            # Font render modes tell the font rasterizer engine what rendering technique to use.
            #
            # Modes availabe are:
            # - lcd          Uses a subpixel rendering technique optimized for LCD displays.
            # - light        Uses a subpixel rendering technique in gray-scale.
            # - gray         Uses standard gray-scaled anti-aliasing.
            # - monochrome   Uses pixel-perfect bitmap rendering.
            render_mode = "lcd";

            # Indicates whether or not to include *only* monospace fonts in the font and
            # font-fallback list (Default: true).
            strict_spacing = true;

            # Font family to use for displaying text.
            #
            # A font can be either described in detail as below or as a
            # simple string value (e.g. "monospace" with teh appropriate
            # weight/slant applied automatically).
            regular = {
              # Font family defines the font family name, such as:
              # Fira Code", "Courier New", or "monospace" (default).
              family = "Cascadia Code";

              # Font weight can be one of:
              #   thin, extra_light, light, demilight, book, normal,
              #   medium, demibold, bold, extra_bold, black, extra_black.
              weight = "regular";

              # Font slant can be one of: normal, italic, oblique.
              slant = "normal";

              # Set of optional font features to be enabled. This
              # is usually a 4-letter code, such as ss01 or ss02 etc.
              #
              # Please see your font's documentation to find out what it
              # supports.
              #
              # Default: []
              features = [ ];
            };

            # If bold/italic/bold_italic are not explicitely specified, the regular font with
            # the respective weight and slant will be used.

            bold = {
              family = "CaskaydiaCove Nerd Font Mono";
              weight = "bold";
              slant = "normal";
            };

            italic = {
              family = "CaskaydiaCove Nerd Font Mono";
              weight = "regular";
              slant = "oblique";
              features = [ "ss02" ];
            };

            bold_italic = {
              family = "CaskaydiaCove Nerd Font Mono";
              weight = "bold";
              slant = "oblique";
              features = [ "ss02" ];
            };

            # This is a special font to be used for displaying unicode symbols
            # that are to be rendered in emoji presentation.
            # Defaults to "emoji".
            emoji = "Noto Color Emoji";
          };

          bold_is_bright = false;

          # Terminal cursor display configuration
          cursor = {
            # Supported shapes are:
            #
            # - block         a filled rectangle
            # - rectangle     just the outline of a block
            # - underscore    a line under the text
            # - bar:          the well known i-Beam
            shape = "bar";

            # Determins whether or not the cursor will be blinking over time.
            blinking = true;

            # Blinking interval (in milliseconds) to use when cursor is blinking.
            blinking_interval = 500;
          };

          # vi-like normal-mode specific settings.
          # Note, currently only the cursor can be customized.
          normal_mode = {
            cursor = {
              shape = "block";
              blinking = false;
              blinking_interval = 500;
            };
          };

          # vi-like visual/visual-line/visual-block mode specific settings.
          # Note, currently only the cursor can be customized.
          visual_mode = {
            cursor = {
              shape = "block";
              blinking = false;
              blinking_interval = 500;
            };
          };

          # Background configuration
          background = {
            # Background opacity to use. A value of 1.0 means fully opaque whereas 0.0 means fully
            # transparent. Only values between 0.0 and 1.0 are allowed.
            opacity = 1;

            # Some platforms can blur the transparent background (currently only Windows 10 is supported).
            blur = false;
          };

          # Specifies a colorscheme to use (alternatively the colors can be inlined).
          colors = "default";

          # If true, bold text is also using bright colors as well.
          #
          # Default: false
          draw_bold_text_with_bright_colors = false;

          # Hyperlinks (via OSC-8) can be stylized and colorized on hover.
          hyperlink_decoration = {
            normal = "dotted";
            hover = "underline";
          };
        };
      };

      # Color Profiles
      # --------------
      #
      # Here you can configure your color profiles, wereas a color can be expressed in standard web format,
      # with a leading # followed by red/green/blue values, 7 characters in total.
      # You may alternatively use 0x as prefix instead of #.
      # For example 0x102030 is equal to '#102030'.
      color_schemes = {
        default = {
          # Default colors
          default = {
            # Default background color (this can be made transparent, see above).
            background = theme.background;

            # Default foreground text color.
            foreground = theme.foreground;
          };

          # Background image support.
          background_image = {
            # Full path to the image to use as background.
            #
            # Default: empty string (disabled)
            # path: '/Users/trapni/Pictures/bg.png'

            # Image opacity to be applied to make the image not look to intense
            # and not get too distracted by the background image.
            # Default: 0.5
            opacity = 1.0;

            # Optionally blurs background image to make it less distracting
            # and keep the focus on the actual terminal contents.
            #
            # Default: false
            blur = false;
          };

          # Mandates the color of the cursor and potentially overridden text.
          #
          # The color can be specified in RGB as usual, plus
          # - CellForeground: Selects the cell's foreground color.
          # - CellBackground: Selects the cell's background color.
          cursor = {
            # Specifies the color to be used for the actual cursor shape.
            #
            # Default: CellForeground
            default = "CellForeground";

            # Specifies the color to be used for the characters that would
            # be covered otherwise.
            #
            # Default: CellBackground
            text = "CellBackground";
          };

          # color to pick for hyperlinks decoration, when hovering
          hyperlink_decoration = {
            normal = "#f0f000";
            hover = "#ff0000";
          };

          # The text selection color can be customized here.
          # Leaving a value empty will default to the inverse of the content's color values.
          selection = {
            # foreground = "#c0c0c0";
            background = theme.selectionBackground;
          };

          # Normal colors
          normal = {
            black = theme.black;
            red = theme.red;
            green = theme.green;
            yellow = theme.yellow;
            blue = theme.blue;
            magenta = theme.purple;
            cyan = theme.cyan;
            white = theme.white;
          };

          # Dim (faint) colors, if not set, they're automatically computed based on normal colors.
          dim = {
            /*
            black = "#1d1f21";
            red = "#cc342b";
            green = "#198844";
            yellow = "#fba922";
            blue = "#3971ed";
            magenta = "#a36ac7";
            cyan = "#3971ed";
            white = "#c5c8c6";
            */
          };

          # Bright colors
          bright = {
            black = theme.brightBlack;
            red = theme.brightRed;
            green = theme.brightGreen;
            yellow = theme.brightYellow;
            blue = theme.brightBlue;
            magenta = theme.brightPurple;
            cyan = theme.brightCyan;
            white = theme.brightWhite;
          };
        };
      };

      # Key Bindings
      # ------------
      #
      # In this section you can customize key bindings.
      # Each array element in `input_mapping` represents one key binding,
      # whereas `mods` represents an array of keyboard modifiers that must be pressed - as well as
      # the `key` or `mouse` -  in order to activate the corresponding action,
      #
      # Additionally one can filter input mappings based on special terminal modes using the `modes` option:
      # - Alt       : The terminal is currently in alternate screen buffer, otherwise it is in primary screen buffer.
      # - AppCursor : The application key cursor mode is enabled (otherwise it's normal cursor mode).
      # - AppKeypad : The application keypad mode is enabled (otherwise it's the numeric keypad mode).
      # - Select    : The terminal has currently an active grid cell selection (such as selected text).
      # - Insert    : The Insert input mode is active, that is the default and one way to test
      #               that the input mode is not in normal mode or any of the visual select modes.
      #
      # You can combine these modes by concatenating them via | and negate a single one
      # by prefixing with ~.
      #
      # The `modes` option defaults to not filter at all (the input mappings always
      # match based on modifier and key press / mouse event).
      #
      # `key` represents keys on your keyboard, and `mouse` represents buttons
      # as well as the scroll wheel.
      #
      # Modifiers:
      # - Alt
      # - Control
      # - Shift
      # - Meta (this is the Windows key on Windows OS, and the Command key on OS/X, and Meta on anything else)
      #
      # Keys can be expressed case-insensitively symbolic:
      #   APOSTROPHE, ADD, BACKSLASH, COMMA, DECIMAL, DIVIDE, EQUAL, LEFT_BRACKET,
      #   MINUS, MULTIPLY, PERIOD, RIGHT_BRACKET, SEMICOLON, SLASH, SUBTRACT, SPACE
      #   Enter, Backspace, Tab, Escape, F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12,
      #   DownArrow, LeftArrow, RightArrow, UpArrow, Insert, Delete, Home, End, PageUp, PageDown,
      #   Numpad_NumLock, Numpad_Divide, Numpad_Multiply, Numpad_Subtract, Numpad_CapsLock,
      #   Numpad_Add, Numpad_Decimal, Numpad_Enter, Numpad_Equal,
      #   Numpad_0, Numpad_1, Numpad_2, Numpad_3, Numpad_4,
      #   Numpad_5, Numpad_6, Numpad_7, Numpad_8, Numpad_9
      # or in case of standard characters, just the character.
      #
      # Mouse buttons can be one of the following self-explanary ones:
      #   Left, Middle, Right, WheelUp, WheelDown
      #
      # Actions:
      # - CancelSelection   Cancels currently active selection, if any.
      # - ChangeProfile     Changes the profile to the given profile `name`.
      # - ClearHistoryAndReset    Clears the history, performs a terminal hard reset and attempts to force a redraw of the currently running application.
      # - CopyPreviousMarkRange   Copies the most recent range that is delimited by vertical line marks into clipboard.
      # - CopySelection     Copies the current selection into the clipboard buffer.
      # - DecreaseFontSize  Decreases the font size by 1 pixel.
      # - DecreaseOpacity   Decreases the default-background opacity by 5%.
      # - FollowHyperlink   Follows the hyperlink that is exposed via OSC 8 under the current cursor position.
      # - IncreaseFontSize  Increases the font size by 1 pixel.
      # - IncreaseOpacity   Increases the default-background opacity by 5%.
      # - NewTerminal       Spawns a new terminal at the current terminals current working directory.
      # - OpenConfiguration Opens the configuration file.
      # - OpenFileManager   Opens the current working directory in a system file manager.
      # - PasteClipboard    Pastes clipboard to standard input.
      # - PasteSelection    Pastes current selection to standard input.
      # - Quit              Quits the application.
      # - ReloadConfig      Forces a configuration reload.
      # - ResetConfig       Overwrites current configuration with builtin default configuration and loads it. Attention, all your current configuration will be lost due to overwrite!
      # - ResetFontSize     Resets font size to what is configured in the config file.
      # - ScreenshotVT      Takes a screenshot in form of VT escape sequences.
      # - ScrollDown        Scrolls down by the multiplier factor.
      # - ScrollMarkDown    Scrolls one mark down (if none present, bottom of the screen)
      # - ScrollMarkUp      Scrolls one mark up
      # - ScrollOneDown     Scrolls down by exactly one line.
      # - ScrollOneUp       Scrolls up by exactly one line.
      # - ScrollPageDown    Scrolls a page down.
      # - ScrollPageUp      Scrolls a page up.
      # - ScrollToBottom    Scrolls to the bottom of the screen buffer.
      # - ScrollToTop       Scrolls to the top of the screen buffer.
      # - ScrollUp          Scrolls up by the multiplier factor.
      # - SendChars         Writes given characters in `chars` member to the applications input.
      # - ToggleAllKeyMaps  Disables/enables responding to all keybinds (this keybind will be preserved when disabling all others).
      # - ToggleFullScreen  Enables/disables full screen mode.
      # - ToggleTitleBar    Shows/Hides titlebar
      # - ViNormalMode      Enters Vi-like normal mode. The cursor can then be moved via h/j/k/l movements and text can be selected via v, yanked via y, and clipboard pasted via p.
      # - WriteScreen       Writes VT sequence in `chars` member to the screen (bypassing the application).

      input_mapping = [
        { mods = [ "Control" ]; mouse = "Left"; action = "FollowHyperlink"; }
        { mods = [ ]; mouse = "Middle"; action = "PasteSelection"; }
        { mods = [ ]; mouse = "WheelDown"; action = "ScrollDown"; }
        { mods = [ ]; mouse = "WheelUp"; action = "ScrollUp"; }
        { mods = [ "Alt" ]; key = "Enter"; action = "ToggleFullscreen"; }
        { mods = [ "Alt" ]; mouse = "WheelDown"; action = "DecreaseOpacity"; }
        { mods = [ "Alt" ]; mouse = "WheelUp"; action = "IncreaseOpacity"; }
        { mods = [ "Control" "Alt" ]; key = "S"; action = "ScreenshotVT"; }
        { mods = [ "Control" "Shift" ]; key = "Plus"; action = "IncreaseFontSize"; }
        { mods = [ "Control" ]; key = "0"; action = "ResetFontSize"; }
        { mods = [ "Control" "Shift" ]; key = "Minus"; action = "DecreaseFontSize"; }
        { mods = [ "Control" "Shift" ]; key = "_"; action = "DecreaseFontSize"; }
        { mods = [ "Control" "Shift" ]; key = "N"; action = "NewTerminal"; }
        { mods = [ "Control" "Shift" ]; key = "C"; action = "CopySelection"; }
        { mods = [ "Control" "Shift" ]; key = "V"; action = "PasteClipboard"; }
        { mods = [ "Control" ]; key = "C"; action = "CopySelection"; mode = "Select|Insert"; }
        { mods = [ "Control" ]; key = "V"; action = "PasteClipboard"; mode = "Select|Insert"; }
        { mods = [ "Control" ]; key = "V"; action = "CancelSelection"; mode = "Select|Insert"; }
        { mods = [ ]; key = "Escape"; action = "CancelSelection"; mode = "Select|Insert"; }
        { mods = [ "Control" "Shift" ]; key = "Space"; action = "ViNormalMode"; mode = "Insert"; }
        { mods = [ "Control" "Shift" ]; key = "Comma"; action = "OpenConfiguration"; }
        { mods = [ "Control" "Shift" ]; key = "Q"; action = "Quit"; }
        { mods = [ "Control" ]; mouse = "WheelDown"; action = "DecreaseFontSize"; }
        { mods = [ "Control" ]; mouse = "WheelUp"; action = "IncreaseFontSize"; }
        { mods = [ "Shift" ]; key = "DownArrow"; action = "ScrollOneDown"; }
        { mods = [ "Shift" ]; key = "End"; action = "ScrollToBottom"; }
        { mods = [ "Shift" ]; key = "Home"; action = "ScrollToTop"; }
        { mods = [ "Shift" ]; key = "PageDown"; action = "ScrollPageDown"; }
        { mods = [ "Shift" ]; key = "PageUp"; action = "ScrollPageUp"; }
        { mods = [ "Shift" ]; key = "UpArrow"; action = "ScrollOneUp"; }
        { mods = [ "Shift" ]; key = "{"; action = "ScrollMarkUp"; mode = "~Alt"; }
        { mods = [ "Shift" ]; key = "}"; action = "ScrollMarkDown"; mode = "~Alt"; }
        { mods = [ "Shift" ]; mouse = "WheelDown"; action = "ScrollPageDown"; }
        { mods = [ "Shift" ]; mouse = "WheelUp"; action = "ScrollPageUp"; }
        { mods = [ "Control" ]; key = "O"; action = "OpenFileManager"; }
      ];
    };
  };
}
