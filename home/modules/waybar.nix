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

  light-border-left =
    pkgs.writeTextFile {
      name = "light-border.svg";
      text =
        ''
          <?xml version="1.0" encoding="UTF-8" standalone="no"?>
          <svg
            width="18"
            height="36"
            viewBox="0 0 4.7624999 9.5249999"
            version="1.1"
            id="svg1"
            xmlns="http://www.w3.org/2000/svg"
            xmlns:svg="http://www.w3.org/2000/svg">
            <defs
              id="defs1" />
            <g
              id="layer1"
              transform="translate(-4.7624999)">
              <circle
                style="fill:${foreground};stroke-width:0.0415811"
                id="path1"
                cx="4.7624998"
                cy="4.7624998"
                r="4.7624998" />
            </g>
          </svg>
        '';
    };

  dark-border-right =
    pkgs.writeTextFile {
      name = "light-border.svg";
      text =
        ''
          <?xml version="1.0" encoding="UTF-8" standalone="no"?>
          <svg
             width="18"
             height="36"
             viewBox="0 0 4.7624999 9.5249999"
             version="1.1"
             id="svg1"
             xmlns="http://www.w3.org/2000/svg"
             xmlns:svg="http://www.w3.org/2000/svg">
            <defs
               id="defs1" />
            <g
               id="layer1">
              <circle
                 style="fill:${black};stroke-width:0.0415811"
                 id="path1"
                 cx="4.7624998"
                 cy="4.7624998"
                 r="4.7624998" />
            </g>
          </svg>
        '';
    };

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
              "height": 36,
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
                "icon-size": 21,
                "spacing": 8
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
            font-size: 16.5pt;
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
            background-color: ${background};
          }

          /** modules-left *********************************************************/

          .modules-left {
            background: ${black};
            
            border-top-right-radius: 18px;
            border-bottom-right-radius: 18px;
          }

          #custom-menu {
            color: ${background};
            background: ${foreground};

            padding-left: calc(18px - 9px);
            padding-right: calc(18px - 8px);

            font-family: 'NotoSansMNerdFontPropo';
          }

          #workspaces {
            color: ${background};
            background: ${foreground};
            padding-left: 2px;
          }

          #workspaces button {
            margin: 0px 4px;

            padding-left: 3px;
            padding-right: 8px;
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
            background-image: url("${light-border-left}");
            background-position: 0% 0%;
            background-repeat: no-repeat;
            background-size: 18px calc(18px * 2);

            padding-left: calc(18px + 10px);
            padding-right: 5px;

            font-family: 'NotoSansMNerdFontPropo';
          }

          #tray {
            background: ${background};

            border-radius: 16px;

            margin: 4px;

            padding: 0px 10px;
          }

          #mode {
            color: ${foreground};

            margin: 0px 4px;
            padding-right: 18px;
          }

          /** modules-center *******************************************************/

          #window {
            color: ${foreground};
            background: ${black};

            border-radius: 18px;

            margin: 2px 0px;

            padding: 0px 16px;
          }

          .empty #window {
            background: transparent;
          }

          /** modules-right ********************************************************/

          .modules-right {
            background: ${black};

            border-top-left-radius: 18px;
            border-bottom-left-radius: 18px;

            padding-left: 18px;
          }

          /* disk */

          #disk {
            color: ${foreground};

            padding-right: calc(18px - 3px);
          }

          /* temperature */

          #temperature {
            color: ${black};
            background: ${foreground};

            border-top-left-radius: 18px;
            border-bottom-left-radius: 18px;

            background-image: url("${dark-border-right}");
            background-position: 100% 0%;
            background-repeat: no-repeat;
            background-size: 18px calc(18px * 2);

            padding-left: 18px;
            padding-right: calc((18px * 2) - 6px);
          }

          /* memory */

          #memory {
            color: ${foreground};
            background: ${black};

            border-top-left-radius: 18px;
            border-bottom-left-radius: 18px;

            padding-right: calc(18px + 4px);
          }

          /* cpu */

          #cpu {
            color: ${black};
            background: ${foreground};

            border-top-left-radius: 18px;
            border-bottom-left-radius: 18px;

            background-image: url("${dark-border-right}");
            background-position: 100% 0%;
            background-repeat: no-repeat;
            background-size: 18px calc(18px * 2);

            padding-left: 18px;
            padding-right: calc(18px * 2);
          }

          /* network */
          #network {
            color: ${foreground};
            background: ${black};

            border-top-left-radius: 18px;
            border-bottom-left-radius: 18px;

            padding-right: calc(18px - 7px);
          }

          /* bluetooth */

          #bluetooth {
            color: ${black};
            background: ${foreground};

            border-top-left-radius: 18px;
            border-bottom-left-radius: 18px;

            background-image: url("${dark-border-right}");
            background-position: 100% 0%;
            background-repeat: no-repeat;
            background-size: 18px calc(18px * 2);

            padding-left: 18px;
            padding-right: calc((18px * 2) - 3px);
          }

          #bluetooth.on {
            color: ${black};
          }

          #bluetooth.connected {
            color: ${blue};
          }

          /* pulseaudio */

          #pulseaudio {
            color: ${foreground};
            background: ${black};

            border-top-left-radius: 18px;
            border-bottom-left-radius: 18px;

            padding-right: calc(18px - 3px);
          }

          /* clock */

          #clock {
            color: ${black};
            background: ${foreground};

            border-top-left-radius: 18px;
            border-bottom-left-radius: 18px;

            padding-left: 18px;
            padding-right: calc(18px - 3px);
          }
        '';
    };
  };
}
