{ config, lib, pkgs, ... }:
let
  inherit (config.modules.rofi) enable;

  settings = pkgs.fetchFromGitHub {
    owner = "chrisfls";
    repo = "rofi";
    rev = "62110b40c0c4a15ae45b6fbc7958d5a5cb6cbf62";
    sha256 = "sha256-R0cr7io8m2z9r0T/4xbVtm5sGVaPDj80GEfXyBTWJI0=";
  };

  wrap = rofi:
    pkgs.writeShellScriptBin "rofi"
      "exec ${rofi} -dpi ${toString config.modules.scaling.dpi-scaled} \"$@\"";

  theme = "${config.xdg.configHome}/rofi/launchers/type-3/style-5.rasi";
  theme' = "${config.xdg.configHome}/rofi/launchers/type-3/style-5-alt.rasi";

  mod = config.xsession.windowManager.i3.config.modifier;

  # rofi calculator
  rofi-calc = pkgs.writeScript "rofi-calc"
    ''
      out=$(
      rofi -theme "${theme'}" \
        -show calc -modi calc -no-show-match -no-sort \
        -calc-command "echo -n '{result}'"
      )
      if [ -n $out ]; then
        echo -n $out | xclip -sel c
      fi
    '';
  # polybar session menu
  rofi-power-menu = pkgs.writeScript "rofi-power-menu"
    ''
      theme="${config.home.homeDirectory}/.config/rofi/powermenu/type-1/style-1.rasi"

      # options
      shutdown=' Shutdown'
      reboot=' Reboot'
      suspend=' Suspend'
      logout=' Logout'
      yes=' Yes'
      no=' No'

      uptime=$(uptime | awk '{ total_minutes=($3 * 60) + $5; hours=int(total_minutes/60); minutes=total_minutes%60; printf("%dh\n", hours) }')
      host=$(cat /etc/hostname)
          
      # run rofi
      rofi_cmd() {
      	rofi -dmenu \
      		-p "$host" \
      		-mesg "Uptime: $uptime" \
      		-theme "$theme"
      }

      # confirm yes/no
      confirm_cmd() {
      	rofi -theme-str 'window {location: center; anchor: center; fullscreen: false; width: 250px;}' \
      		-theme-str 'mainbox {children: [ "message", "listview" ];}' \
      		-theme-str 'listview {columns: 2; lines: 1;}' \
      		-theme-str 'element-text {horizontal-align: 0.5;}' \
      		-theme-str 'textbox {horizontal-align: 0.5;}' \
      		-dmenu \
      		-p 'Confirmation' \
      		-mesg 'Are you Sure?' \
      		-theme "$theme"
      }

      # perform
      run_cmd() {
      	if [[ "$(echo -e "$yes\n$no" | confirm_cmd)" == "$yes" ]]; then
      		if [[ $1 == '--shutdown' ]]; then
      			systemctl poweroff
      		elif [[ $1 == '--reboot' ]]; then
      			systemctl reboot
      		elif [[ $1 == '--suspend' ]]; then
      			# mpc -q pause
      			amixer set Master mute
      			systemctl suspend
      		elif [[ $1 == '--logout' ]]; then
      			i3-msg exit
      		fi
      	else
      		exit 0
      	fi
      }

      # actions
      case "$(echo -e "$suspend\n$logout\n$reboot\n$shutdown" | rofi_cmd)" in
        $shutdown)
          run_cmd --shutdown
          ;;
        $reboot)
          run_cmd --reboot
          ;;
        $suspend)
          run_cmd --suspend
          ;;
        $logout)
          run_cmd --logout
          ;;
      esac
    '';

  # run menu
  rofi-run = pkgs.writeScript "rofi-run"
    "exec rofi -show run -theme \"${theme}\"";

  # global windows
  rofi-windows = pkgs.writeScript "rofi-windows"
    "exec rofi -modi window -show window -theme \"${theme}\"";
in
{
  options.modules.rofi.enable = lib.mkEnableOption "Enable rofi module";

  config = lib.mkIf enable {
    pacman.packages = [ "extra/rofi" "extra/rofi-calc" ];

    modules.i3wm = {
      run = "rofi-run";
      menu = "rofi-menu";
      power-menu = "rofi-power-menu";
      window-list = "rofi-windows";
      calculator = "rofi-calc";
    };

    home.packages = [
      (wrap "/usr/bin/rofi")

      # rofi notifications menu, not dash because arrays
      (pkgs.writeShellScriptBin "rofi-dunst"
        ''
          history=$(dunstctl history | jq -r .data[][])
          ids=($(echo $history | jq -r .id.data))
          summaries=("$(echo $history | jq -r .summary.data)" "󰃢 Clear All")
          selected=$(
            printf "%s\n" "''${summaries[@]}" | grep -v '^$' \
              | rofi -dmenu -theme \"${theme'}\" -format i -p " "
          )
          if [[ -n $selected ]]; then
            if [[ $selected -lt ''${#summaries[@]} ]]; then
              dunstctl history-pop "''${ids[$selected]}"
            else
              for id in "''${ids[@]}"; do
                dunstctl history-rm "$id"
              done
            fi
          fi
        '')

      # polybar main menu
      (pkgs.writeShellScriptBin "rofi-menu"
        "exec rofi -show drun -theme \"${theme}\"")
    ];

    xdg.dataFile = {
      "fonts/GrapeNuts-Regular.ttf".source = "${settings}/fonts/GrapeNuts-Regular.ttf";
      "fonts/Icomoon-Feather.ttf".source = "${settings}/fonts/Icomoon-Feather.ttf";
      "fonts/Iosevka-Nerd-Font-Complete.ttf".source = "${settings}/fonts/Iosevka-Nerd-Font-Complete.ttf";
      "fonts/JetBrains-Mono-Nerd-Font-Complete.ttf".source = "${settings}/fonts/JetBrains-Mono-Nerd-Font-Complete.ttf";
    };

    xdg.configFile."rofi".source = "${settings}/files";
  };
}
