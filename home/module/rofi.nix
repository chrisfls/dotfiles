{ config, lib, pkgs, ... }:
let
  inherit (config.module.rofi) enable;

  settings = pkgs.fetchFromGitHub {
    owner = "kress95";
    repo = "rofi";
    rev = "6d280453c6a4d5baf284bc29dcb922567dc2b9ec";
    sha256 = "sha256-ucUAFI5O+wYGkRqNSdkve9j21Xguzn3WhizzXNBSJ/0=";
  };

  xtitle = "${pkgs.xtitle}/bin/xtitle";

  theme = "${config.xdg.configHome}/rofi/launchers/type-3/style-5-alt.rasi";

  mod = config.xsession.windowManager.i3.config.modifier;

  # TODO: port to module.script.install

  # rofi calculator
  rofi-calc = pkgs.writeShellScriptBin "rofi-calc"
    ''
      out=$(
      rofi -theme "${theme}" \
        -show calc -modi calc -no-show-match -no-sort \
        -calc-command "echo -n '{result}'"
      )
      if [[ -n $out ]]; then
        echo -n $out | xclip -sel c
      fi
    '';

  # rofi notifications menu
  rofi-dunst = pkgs.writeShellScriptBin "rofi-dunst"
    ''
      history=$(dunstctl history | jq -r .data[][])
      ids=($(echo $history | jq -r .id.data))
      summaries=("$(echo $history | jq -r .summary.data)" "󰃢 Clear All")
      selected=$(
        printf "%s\n" "$\{summaries[@]}" | grep -v '^$' \
          | rofi -dmenu -theme "${theme}" -format i -p " "
      )
      if [[ -n $selected ]]; then
        if [[ $selected -lt $\{#summaries[@]} ]]; then
          dunstctl history-pop "$\{ids[$selected]}"
        else
          for id in "$\{ids[@]}"; do
            dunstctl history-rm "$id"
          done
        fi
      fi
    '';

  # polybar main menu
  rofi-menu = pkgs.writeShellScriptBin "rofi-menu"
    "rofi -show drun -theme \"${config.module.themes.rofi}\"";

  # polybar session menu
  rofi-power-menu = pkgs.writeShellScriptBin "rofi-power-menu" "${config.xdg.configHome}/rofi/powermenu/type-1/powermenu.sh";

  # run menu
  rofi-run = pkgs.writeShellScriptBin "rofi-run"
    ''
      rofi -show run -theme "${theme}"
    '';

  # global windows
  rofi-windows = pkgs.writeShellScriptBin "rofi-windows"
    ''
      rofi -modi window -show window -theme "${theme}"
    '';

  # current desktop windows
  rofi-windows-cd = pkgs.writeShellScriptBin "rofi-windows-cd"
    ''
      rofi -modi windowcd -show windowcd -theme "${theme}"
    '';

  /*
    # minimized windows TODO: add process name or window class
    rofi-windows-minimized = pkgs.writeShellScriptBin "rofi-windows-minimized"
    ''
      ids=($(bspc query --nodes --node '.hidden.local.window'))
      if [ -z "$ids" ]; then exit 0; fi

      names=$(${xtitle} $\{ids[@]})
      selected=$(
        printf "%s\n" "$\{names[@]}" | grep -v '^$' \
          | rofi -dmenu -theme "${theme}" -format i -p " "
      )
      if [ "$selected" ]; then
          bspc node "$\{ids[$selected]}" --flag hidden=off
      fi
    '';

    # minimized windows TODO: add process name or window class
    rofi-windows-scratchpad = pkgs.writeShellScriptBin "rofi-windows-scratchpad"
    ''
      ids=($(bspc query --nodes --desktop 'pad' --node '.window'))
      if [ -z "$ids" ]; then exit 0; fi

      names=$(${xtitle} $\{ids[@]})
      selected=$(
        printf "%s\n" "$\{names[@]}" | grep -v '^$' \
          | rofi -dmenu -theme "${theme}" -format i -p " "
      )
      if [ "$selected" ]; then
        bspc node "$\{ids[$selected]}" --to-desktop focused --follow
      fi
    '';
  */

in
{
  options.module.rofi.enable = lib.mkEnableOption "Enable rofi module";

  config = lib.mkIf enable {
    home.packages = [
      pkgs.usr.rofi
      pkgs.usr.rofi-calc
      pkgs.hostname # TODO: update rofi theme to not need this
      rofi-calc
      rofi-dunst
      rofi-menu
      rofi-power-menu
      rofi-run
      rofi-windows
      rofi-windows-cd
      # rofi-windows-minimized
      # rofi-windows-scratchpad
    ];

    pacman.usr = {
      rofi = [ "extra/rofi" ];
      rofi-calc = [ "extra/rofi-calc" ];
    };

    # module.sxhkd.keybindings = {
    #   # main menus
    #   "super + Return" = "rofi-menu";
    #   "super + shift + Return" = "rofi-run";

    #   # jump to window
    #   "super + w" = "rofi-windows-cd";
    #   "super + shift + w" = "rofi-windows";

    #   # jump to minimized windows
    #   "super + alt + m" = "rofi-windows-minimized";
    #   # jump to scratchpad windows
    #   "super + alt + n" = "rofi-windows-scratchpad";

    #   # shift cmd for alacritty
    #   "super + shift + BackSpace" = "rofi-calc";
    #   "super + shift + semicolon" = "rofi-calc";
    # };

    xsession.windowManager.i3.config = {
      menu = "rofi-menu";
      keybindings = {
        # main menus
        "${mod}+Shift+Return" = "exec rofi-run";

        # jump to window
        "${mod}+w" = "exec rofi-windows-cd";
        "${mod}+Shift+w" = "exec rofi-windows";

        # Shift cmd for alacritty
        "${mod}+Shift+BackSpace" = "exec rofi-calc";
        "${mod}+Shift+semicolon" = "exec rofi-calc";
      };
    };

    xdg.dataFile = {
      "fonts/GrapeNuts-Regular.ttf".source = "${settings}/fonts/GrapeNuts-Regular.ttf";
      "fonts/Icomoon-Feather.ttf".source = "${settings}/fonts/Icomoon-Feather.ttf";
      "fonts/Iosevka-Nerd-Font-Complete.ttf".source = "${settings}/fonts/Iosevka-Nerd-Font-Complete.ttf";
      "fonts/JetBrains-Mono-Nerd-Font-Complete.ttf".source = "${settings}/fonts/JetBrains-Mono-Nerd-Font-Complete.ttf";
    };

    xdg.configFile."rofi".source = "${settings}/files";
  };
}
