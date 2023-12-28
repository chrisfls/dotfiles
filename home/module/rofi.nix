{ config, lib, pkgs, ... }:
let
  cfg = config.module.rofi;

  settings = pkgs.fetchFromGitHub {
    owner = "kress95";
    repo = "rofi";
    rev = "6d280453c6a4d5baf284bc29dcb922567dc2b9ec";
    sha256 = "sha256-ucUAFI5O+wYGkRqNSdkve9j21Xguzn3WhizzXNBSJ/0=";
  };

  dpi = builtins.toString (builtins.floor config.module.scaling.dpiScaled);

  pkg =
    let
      pkg = pkgs.rofi.override { plugins = [ pkgs.rofi-calc ]; };
    in
    pkgs.symlinkJoin {
      name = "rofi";
      paths = [
        (pkgs.writeShellScriptBin "rofi" "${pkg}/bin/rofi -dpi ${dpi} \"$@\"")
        pkg
      ];
    };

  xtitle = "${pkgs.xtitle}/bin/xtitle";

  theme = "${config.xdg.configHome}/rofi/launchers/type-3/style-5-alt.rasi";

  dollar = "$";

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
        printf "%s\n" "${dollar}{summaries[@]}" | grep -v '^$' \
          | rofi -dmenu -theme "${theme}" -format i -p " "
      )
      if [[ -n $selected ]]; then
        if [[ $selected -lt ${dollar}{#summaries[@]} ]]; then
          dunstctl history-pop "${dollar}{ids[$selected]}"
        else
          for id in "${dollar}{ids[@]}"; do
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

  # minimized windows TODO: add process name or window class
  rofi-windows-minimized = pkgs.writeShellScriptBin "rofi-windows-minimized"
    ''
      ids=($(bspc query --nodes --node '.hidden.local.window'))
      if [ -z "$ids" ]; then exit 0; fi

      names=$(${xtitle} ${dollar}{ids[@]})
      selected=$(
        printf "%s\n" "${dollar}{names[@]}" | grep -v '^$' \
          | rofi -dmenu -theme "${theme}" -format i -p " "
      )
      if [ "$selected" ]; then
          bspc node "${dollar}{ids[$selected]}" --flag hidden=off
      fi
    '';

  # minimized windows TODO: add process name or window class
  rofi-windows-scratchpad = pkgs.writeShellScriptBin "rofi-windows-scratchpad"
    ''
      ids=($(bspc query --nodes --desktop 'pad' --node '.window'))
      if [ -z "$ids" ]; then exit 0; fi

      names=$(${xtitle} ${dollar}{ids[@]})
      selected=$(
        printf "%s\n" "${dollar}{names[@]}" | grep -v '^$' \
          | rofi -dmenu -theme "${theme}" -format i -p " "
      )
      if [ "$selected" ]; then
        bspc node "${dollar}{ids[$selected]}" --to-desktop focused --follow
      fi
    '';

in
{
  options.module.rofi.enable = lib.mkEnableOption "Enable rofi module";

  config = lib.mkIf cfg.enable {
    home.packages = [
      pkg
      pkgs.hostname
      rofi-calc
      rofi-dunst
      rofi-menu
      rofi-power-menu
      rofi-run
      rofi-windows
      rofi-windows-cd
      rofi-windows-minimized
      rofi-windows-scratchpad
    ];

    module.sxhkd.keybindings = {
      # main menus
      "super + Return" = "rofi-menu";
      "super + shift + Return" = "rofi-run";

      # jump to window
      "super + v" = "rofi-windows-cd";
      "super + shift + v" = "rofi-windows";

      # jump to minimized windows
      "super + alt + m" = "rofi-windows-minimized";
      # jump to scratchpad windows
      "super + alt + n" = "rofi-windows-scratchpad";

      # shift cmd for alacritty
      "super + shift + BackSpace" = "rofi-calc";
      "super + shift + semicolon" = "rofi-calc";
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
