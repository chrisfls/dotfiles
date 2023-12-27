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

  # rofi calculator
  rofi-calc = pkgs.writeShellScriptBin "rofi-calc"
    ''
      out=$(
      rofi -theme "/home/kress/.config/rofi/launchers/type-3/style-5-alt.rasi" \
        -show calc -modi calc -no-show-match -no-sort \
        -calc-command "echo -n '{result}'"
      )
      if [[ -n $out ]]; then
        echo -n $out | xclip -sel c
      fi
    '';

  dollar = "$";

  # rofi notifications menu
  rofi-dunst = pkgs.writeShellScriptBin "rofi-dunst"
    ''
      history=$(dunstctl history | jq -r .data[][])
      ids=($(echo $history | jq -r .id.data))
      summaries=("$(echo $history | jq -r .summary.data)" "󰃢 Clear All")
      selected=$(
        printf "%s\n" "${dollar}{summaries[@]}" | grep -v '^$' \
          | rofi -dmenu -theme "/home/kress/.config/rofi/launchers/type-3/style-5-alt.rasi" -format i -p " "
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
  rofi-mainmenu = pkgs.writeShellScriptBin "rofi-mainmenu"
    "rofi -show drun -theme \"${config.module.themes.rofi}\"";

  # polybar session menu
  rofi-powermenu = pkgs.writeShellScriptBin "rofi-powermenu" "${config.xdg.configHome}/rofi/powermenu/type-1/powermenu.sh";

  # rofi run menu
  rofi-run = pkgs.writeShellScriptBin "rofi-run"
    ''
      rofi -show run -theme "/home/kress/.config/rofi/launchers/type-3/style-5-alt.rasi"
    '';

  # rofi windows menu
  rofi-windows = pkgs.writeShellScriptBin "rofi-windows"
    ''
      rofi -show window -theme "/home/kress/.config/rofi/launchers/type-3/style-5-alt.rasi"
    '';

  user = config.home.username;
in
{
  options.module.rofi.enable = lib.mkEnableOption "Enable rofi module";

  config = lib.mkIf cfg.enable {
    home.packages = [
      pkg
      pkgs.hostname
      rofi-calc
      rofi-dunst
      rofi-mainmenu
      rofi-powermenu
      rofi-run
      rofi-windows
    ];

    module.sxhkd.keybindings = {
      "super + apostrophe" = "rofi-windows";
      "super + Return" = "rofi-mainmenu";
      "super + shift + BackSpace" = "rofi-calc";
      "super + shift + Return" = "rofi-run";
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
