# TODO: handle scaling
{ config, lib, pkgs, specialArgs, ... }:
let
  inherit (config.modules.waybar) enable;

  background = "#232627"; # backgroundBright / blackAbsolute
  background-alpha = "rgba(35,38,39,0.9)";
  white = "#FFFFFF";
  foreground = "#FCFCFC"; # foreground / white
  black = "#31363B"; # background / black
  red = "#E77E8E"; # red
  blue = "#3DAEE9"; # blue
  yellow = "#E08B4A"; # yellow

  dunst-toggle = pkgs.writeScript "bluetooth-toggle"
    ''
      #!/usr/bin/env bash
      set -euo pipefail

      readonly ENABLED=''
      readonly DISABLED=''

      dbus-monitor path='/org/freedesktop/Notifications',interface='org.freedesktop.DBus.Properties',member='PropertiesChanged' --profile |
        while read -r _; do
          PAUSED="$(dunstctl is-paused)"
          if [ "$PAUSED" == 'false' ]; then
            CLASS="enabled"
            TEXT="$ENABLED"
          else
            CLASS="disabled"
            TEXT="$DISABLED"
          fi
          printf '{"text": "%s", "class": "%s"}\n' "$TEXT" "$CLASS"
        done
    '';

  bluetooth-toggle = pkgs.writeScript "bluetooth-toggle"
    ''
      #!/usr/bin/env bash
      if bluetoothctl show | grep -q "Powered: yes"; then
        bluetoothctl power off
      else
        bluetoothctl power on
      fi
    '';
in
{
  options.modules.waybar.enable = lib.mkEnableOption "Enable waybar module";

  config = lib.mkIf enable {
    pacman.packages = [
      "extra/waybar"
    ];

    modules.sway.startup = [ "systemctl --user restart waybar.service" ];

    xdg.configFile."waybar/config" = {
      text =
        ''
          [
            {
              "layer": "bottom",
              "position": "top",
              "height": 24,
              "margin-left": 0,
              "margin-right": 0,
              "output": [
                "HDMI-A-1"
              ],
              /** modules-left *********************************************************/
              "modules-left": [
                "custom/menu",
                "sway/workspaces",
                "custom/dunst",
                "tray",
                "sway/mode"
              ],
              "sway/workspaces": {
                "disable-scroll": true,
                "all-outputs": true,
                "format": "{icon}",
                "format-icons": {
                  "0": "󰪥",
                  "1": "󰲠",
                  "2": "󰲢",
                  "3": "󰲤",
                  "4": "󰲦",
                  "5": "󰲨",
                  "6": "󰲪",
                  "7": "󰲬",
                  "8": "󰲮",
                  "9": "󰲰",
                  "10": "󰲞",
                  "default": "󰝦"
                }
              },
              "sway/mode": {
                "format": "{}",
              },
              "custom/dunst": {
                "return-type": "json",
                "exec": "${dunst-toggle}",
                "on-click": "dunstctl set-paused toggle",
                "on-scroll-up": "dunstctl close",
                "on-scroll-down": "dunstctl history-pop",
                "restart-interval": 1
              },
              "custom/menu": {
                "format": "",
                "on-click": "rofi-menu"
              },
              "tray": {
                "icon-size": 14,
                "spacing": 5
              },
              /** modules-center *******************************************************/
              "modules-center": [
                "sway/window"
              ],
              "sway/window": {
                "format": "{title}",
                "all-outputs": true,
                "icon": false,
                "justify": "left"
              },
              /** modules-right ********************************************************/
              "modules-right": [
                "disk",
                "temperature",
                "memory",
                "cpu",
                "network",
                "bluetooth",
                "pulseaudio",
                "clock"
              ],
              "custom/rsep": {
                "format": " ",
                "max-length": 1
              },
              "disk": {
                "format": "{free} 󰋊",
                "path": "/",
                "interval": 30,
                "tooltip": false
              },
              "temperature": {
                "format": "{temperatureC}°C ",
                "thermal-zone": 7, // same as thermal-zone=0 in polybar
                "critical-threshold": 80,
                "interval": 1,
                "tooltip": false
              },
              "memory": {
                "format": "{}% ",
                "interval": 2,
                "tooltip": false
              },
              "cpu": {
                "format": "{icon} ",
                "format-icons": [
                  "▁",
                  "▂",
                  "▃",
                  "▄",
                  "▅",
                  "▆",
                  "▇",
                  "<span color='${red}'>█</span>"
                ],
                "interval": 2
              },
              "network": {
                "format": "{bandwidthTotalBytes} 󰈁",
                "format-wifi": "{bandwidthTotalBytes} {signalStrength}% ",
                "format-disconnected": "off 󰈂",
                "tooltip-format": "{ipaddr} at {ifname}",
                "tooltip-format-wifi": "{ipaddr} at {essid} ",
                "tooltip-format-disconnected": "Disconnected",
                "interval": 5
              },
              "bluetooth": {
                "format": "󰂲",
                "format-on": "",
                "format-disabled": "󰂲",
                "format-connected": "",
                "tooltip-format": "{controller_alias}\t{controller_address}",
                "tooltip-format-connected": "{controller_alias}\t{controller_address}\n\n{device_enumerate}",
                "tooltip-format-enumerate-connected": "{device_alias}\t{device_address}",
                "on-click": "${bluetooth-toggle}",
                "on-click-right": "bluedevil-wizard",
                "interval": 1
              },
              "pulseaudio": {
                "format": "{volume}% {icon}",
                "format-muted": "{volume}% 󰝟",
                "format-icons": {
                  "default": [
                    "󰕾"
                  ]
                },
                "scroll-step": 5,
                "on-click": "pamixer --toggle-mute",
                "on-click-right": "pavucontrol-qt"
              },
              "clock": {
                "format": "{:%H:%M:%S} 󰥔",
                "format-alt": "{:%a, %d %b %Y} 󰃭",
                "tooltip-format": "<tt><small>{calendar}</small></tt>",
                "calendar": {
                  "mode": "year",
                  "mode-mon-col": 3,
                  "weeks-pos": "left",
                  "format": {
                    "months": "<span color='${white}'><b>{}</b></span>",
                    "days": "<span color='${foreground}'><b>{}</b></span>",
                    "weeks": "<span color='${red}'><b>W{}</b></span>",
                    "weekdays": "<span color='${yellow}'><b>{}</b></span>",
                    "today": "<span color='${blue}'><b><u>{}</u></b></span>"
                  },
                  "on-scroll": 1,
                  "on-click-right": "mode"
                },
                "actions": {
                  "on-scroll-up": "shift_up",
                  "on-scroll-down": "shift_down"
                },
                "interval": 1
              }
            }
          ]
        '';
    };

    xdg.configFile."waybar/style.css" = {
      text =
        ''
          * {
            all: unset;
            font-size: 11pt;
            font-family: 'NotoSansMNerdFont';
          }

          window#waybar {
            background: ${background-alpha};
          }

          tooltip {
            color: ${foreground};
            background-color: ${background-alpha};

            border-radius: 2px;

            padding: 4px;
          }

          #tray menu {
            color: ${foreground};
            background-color: ${background-alpha};

            border-radius: 2px;

            padding: 4px;
          }

          #tray menu  *:hover {
            color: ${background};
            background-color: ${foreground};
          }

          /** modules-left *********************************************************/

          .modules-left {
            background: ${black};
            
            border-bottom-right-radius: 16px;
            padding-right: 16px;
          }

          #custom-menu {
            color: ${background};
            background: ${foreground};

            padding-left: calc(16px - 9px);
            padding-right: calc(16px - 8px);

            font-family: 'NotoSansMNerdFontPropo';
          }

          #workspaces {
            color: ${background};
            background: ${foreground};

            padding-left: 2px;
            padding-right: 8px;

            border-bottom-right-radius: 16px;
          }

          #workspaces button {
            margin: 0px 1px;

            padding-left: 2px;
            padding-right: 5px;
          }

          #workspaces button#sway-workspace-0 {
            margin-left: 0px;
          }

          #workspaces button#sway-workspace-10 {
            margin-right: 0px;
          }

          #workspaces button.visible {
            color: ${black};
          }

          #workspaces button.focused {
            color: ${foreground};
            text-shadow:
              1px 1px 0 ${black},
              -1px 1px 0 ${black},
              -1px -1px 0 ${black},
              1px -1px 0 ${black};
          }

          #workspaces button.urgent {
            color: ${yellow};
          }

          #custom-dunst {
            padding-left: 4px;
            padding-right: 4px;
            margin-left: 4px;

            font-family: 'NotoSansMNerdFontPropo';
          }

          #tray {
            color: ${foreground};
            margin-left: 2px;
          }

          #mode {
            color: ${foreground};
            padding-left: 8px;
          }

          /** modules-center *******************************************************/

          #window {
            color: ${foreground};
            background: ${black};

            border-radius: 8px;
            margin: 4px 0px;
            padding: 0px 8px;
          }

          .empty #window {
            background: transparent;
          }

          /** modules-right ********************************************************/

          .modules-right {
            background: ${black};

            border-bottom-left-radius: 16px;

            padding-left: 16px;
          }

          /* disk */

          #disk {
            color: ${foreground};

            padding-right: 16px;
          }

          /* temperature */

          #temperature {
            color: ${foreground};
            background: ${background};

            padding: 0px 16px;

            border-radius: 12px;
            margin: 2px 0px;
          }

          /* memory */

          #memory {
            color: ${foreground};
            background: ${black};

            padding: 0px 16px;
          }

          /* cpu */

          #cpu {
            color: ${foreground};
            background: ${background};

            padding: 0px 16px;

            border-radius: 12px;
            margin: 2px 0px;
          }

          /* network */
          #network {
            color: ${foreground};
            background: ${black};

            padding: 0px 16px;
          }

          /* bluetooth */

          #bluetooth {
            color: ${foreground};
            background: ${background};

           

            border-radius: 12px;            padding: 0px 16px;
            
            margin: 0px 2px;
          }

          #bluetooth.on {
            color: ${foreground};
          }

          #bluetooth.connected {
            color: ${blue};
          }

          /* pulseaudio */

          #pulseaudio {
            color: ${foreground};
            background: ${black};

            padding: 0px 16px;
          }

          /* clock */

          #clock {
            color: ${background};
            background: ${foreground};

            border-bottom-left-radius: 16px;

            padding-left: 16px;
            padding-right: calc(16px - 3px);
          }
        '';
    };
  };
}
