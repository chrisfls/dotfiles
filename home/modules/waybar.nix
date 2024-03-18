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
            COUNT="$(dunstctl count waiting)"
            if [ "$COUNT" != '0' ]; then
              TEXT="$DISABLED ($COUNT)"
            fi
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
                "sway/mode",
                "custom/dunst",
                "tray"
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
                "restart-interval": 1
              },
              "custom/menu": {
                "format": "",
                "on-click": "rofi-menu"
              },
              "tray": {
                "icon-size": 20,
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
          	font-size: 11pt;
          	font-family: 'CaskaydiaCoveNerdFont', monospace;
          }


          window#waybar {
          	background: ${background-alpha};
            
          }

          tooltip {
          	border-radius: 2px;
          	background-color: ${background-alpha};
            padding: 4px;
          }

          /** modules-left *********************************************************/

          .modules-left {
          	background: ${black};
          	
          	border-top-right-radius: 18px;
          	border-bottom-right-radius: 18px;
          	
          	padding-right: 18px;
          }

          #custom-menu {
          	background: ${foreground};
          	color: ${background};

          	padding-left: 10px;
          	padding-right: 10px;

          	font-family: 'CaskaydiaCoveNerdFontPropo';
          }

          #workspaces {
          	background: ${foreground};
          	color: ${background};
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

          #mode {
          	background: ${foreground};
          	color: ${background};

          	padding-left: 10px;
          }

          #custom-dunst {
          	background-image: url("${light-border-left}");
          	background-repeat: no-repeat;
          	background-position: 0% 0%;
          	background-size: 18px 36px;

          	padding-left: calc(18px + 10px);

          	font-family: 'CaskaydiaCoveNerdFontPropo';
          }

          #tray {
          	background: ${background};
          	margin: 3px 0px;
          	margin-left: 14px;
          	padding: 0px 10px;
          	border-radius: 16px;
          }

          /** modules-center *******************************************************/

          #window {
          	background: ${black};
          	color: ${foreground};

          	margin: 3px 0px;
          	padding: 0px 16px;
          	border-radius: 18px;
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
          	background: ${black};
          	color: ${foreground};

          	border-top-left-radius: 18px;
          	border-bottom-left-radius: 18px;

          	padding-left: 4px;
          	padding-right: 18px;

          	/*margin-right: 10px;*/
          }

          /* temperature */

          #temperature {
          	background: ${foreground};
          	color: ${black};

          	border-top-left-radius: 18px;
          	border-bottom-left-radius: 18px;

          	background-image: url("${dark-border-right}");
          	background-repeat: no-repeat;
          	background-position: 100% 0%;
          	background-size: 18px 36px;

          	padding-left: 18px;
          	padding-right: calc(18px + 14px);

          	/*margin-right: 10px;*/
          }

          /* memory */

          #memory {
          	background: ${black};
          	color: ${foreground};

          	border-top-left-radius: 18px;
          	border-bottom-left-radius: 18px;

          	padding-left: 4px;
          	padding-right: 18px;

          }

          /* cpu */

          #cpu {
          	background: ${foreground};
          	color: ${black};

          	border-top-left-radius: 18px;
          	border-bottom-left-radius: 18px;

          	background-image: url("${dark-border-right}");
          	background-repeat: no-repeat;
          	background-position: 100% 0%;
          	background-size: 18px 36px;

          	padding-left: 18px;
          	padding-right: calc(18px + 14px);
          }

          /* network */
          #network {
          	background: ${black};
          	color: ${foreground};

          	border-top-left-radius: 18px;
          	border-bottom-left-radius: 18px;

          	padding-left: 4px;
          	padding-right: 18px;
          }

          /* bluetooth */

          #bluetooth {
          	background: ${foreground};
          	color: ${black};

          	border-top-left-radius: 18px;
          	border-bottom-left-radius: 18px;

          	background-image: url("${dark-border-right}");
          	background-repeat: no-repeat;
          	background-position: 100% 0%;
          	background-size: 18px 36px;

          	padding-left: 18px;
          	padding-right: calc(18px + 14px);
          }

          #bluetooth.on {
          	color: ${black};
          }

          #bluetooth.connected {
          	color: ${blue};
          }

          /* pulseaudio */

          #pulseaudio {
          	background: ${black};
          	color: ${foreground};

          	border-top-left-radius: 18px;
          	border-bottom-left-radius: 18px;

          	padding-left: 4px;
          	padding-right: 18px;
          }

          /* clock */

          #clock {
          	background: ${foreground};
          	color: ${black};

          	border-top-left-radius: 18px;
          	border-bottom-left-radius: 18px;

          	padding-left: 18px;
          	padding-right: 12px;
          }
        '';
    };
  };
}
