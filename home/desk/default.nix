{ config, pkgs, ... }:
let
  themes = import ./themes.nix;
  theme = themes.popping-and-locking;
in
{
  fonts.fontconfig.enable = true;

  services.easyeffects.enable = true;

  home.packages = with pkgs; [
    rofi-wayland
    hyprpaper
    networkmanagerapplet

    wl-clipboard

    # screenshot
    grimblast

    dunst # notifications
    pamixer # used by bar

    libsForQt5.audiotube

    languagetool
    xdg-desktop-portal-hyprland
    webcord-vencord
    alsa-utils
    lxqt.pcmanfm-qt
    lxqt.lxqt-sudo
    lxqt.lxqt-sudo
    lxqt.lxqt-notificationd
    lxqt.lxqt-archiver
    lxqt.lximage-qt
    lxqt.qps
    lxqt.screengrab
    lxqt.pavucontrol-qt
    lxqt.lxqt-powermanagement
    lxqt.lxqt-policykit
    lxqt.lxqt-openssh-askpass
  ];

  #Iwayland.windowManager.hyprland.package

  programs.waybar =
    let
      u = str: builtins.fromJSON "\"${str}\"";
      withVars = attrs: str:
        builtins.replaceStrings
          (map (key: "\$${key}") (builtins.attrNames attrs))
          (builtins.attrValues attrs)
          str;
    in
    with theme; {
      enable = true;
      settings = {
        mainBar = {
          layer = "top";
          height = 24;
          exclusive = true;
          margin-left = 16;
          margin-right = 16;
          modules-left = [
            "hyprland/workspaces"
            "hyprland/submap"
          ];
          modules-center = [
            "hyprland/window"
          ];
          modules-right = [
            "tray"
            "group/perf"
            "group/usage"
            "group/conn"
            "keyboard-state"
            "group/media"
            "clock#date"
            "clock#time"
          ];
          # HYPRLAND -------- ---- -- -
          "hyprland/workspaces" = {
            format = "{icon}";
            on-click = "activate";
            persistent-workspaces = {
              "1" = [ ];
              "2" = [ ];
              "3" = [ ];
              "4" = [ ];
              "5" = [ ];
              "6" = [ ];
              "7" = [ ];
              "8" = [ ];
              "9" = [ ];
              "10" = [ ];
            };
            format-icons = {
              "1" = "1";
              "2" = "2";
              "3" = "3";
              "4" = "4";
              "5" = "5";
              "6" = "6";
              "7" = "7";
              "8" = "8";
              "9" = "9";
              "10" = "0";
            };
          };
          "hyprland/window" = {
            format = "{}";
            separate-outputs = true;
          };
          "hyprland/submap" = {
            format = u "\\uf065";
            tooltip = false;
          };
          # TRAY -------- ---- -- -
          tray = {
            icon-size = 16;
            spacing = 8;
          };
          # GROUP-PERF -------- ---- -- -
          "group/perf" = {
            orientation = "horizontal";
            modules = [
              "cpu"
              "temperature"
            ];
            drawer.transition-left-to-right = false;
          };
          cpu = {
            interval = 1;
            format = "{icon} ${u "\\uf4bc"}";
            format-icons = [
              "<span color='${green}'>▁</span>"
              "<span color='${green}'>▂</span>"
              "<span color='${yellow}'>▃</span>"
              "<span color='${yellow}'>▄</span>"
              "<span color='${yellow}'>▅</span>"
              "<span color='${red}'>▆</span>"
              "<span color='${red}'>▇</span>"
              "<span color='${red}'>█</span>"
            ];
          };
          temperature = {
            format = "{temperatureC}°C ${u "\\uf2c7"}";
            # TODO: add other sensors if made available
            tooltip = false;
          };
          # GROUP-USAGE -------- ---- -- -
          "group/usage" = {
            orientation = "horizontal";
            modules = [
              "memory"
              "disk"
            ];
            drawer.transition-left-to-right = false;
          };
          memory = {
            interval = 5;
            format = "{}% ${u "\\udb80\\udf5b"} ${u"\\ue621"} {swapPercentage}% ${u "\\udb82\\udfcd"}";
          };
          disk = {
            interval = 60;
            format = "{percentage_used}%  ${u "\\uf0a0"}";
            path = "/";
            tooltip-format = "{used} / {total}";
          };
          # GROUP-CONNECTION -------- ---- -- -
          "group/conn" = {
            orientation = "horizontal";
            modules = [
              "network"
              "bluetooth"
            ];
            drawer.transition-left-to-right = false;
          };
          network = {
            interval = 1;
            format = "{ipaddr}/{cidr}  ${u "\\udb81\\udd9f"}";
            format-link = "${u "\\udb81\\udf0f"}";
            format-disconnected = "${u "\\udb82\\ude8e"}";
            format-icons = [
              (u "\\udb82\\udd2f")
              (u "\\udb82\\udd1f")
              (u "\\udb82\\udd22")
              (u "\\udb82\\udd25")
              (u "\\udb82\\udd28")
            ];
            tooltip-format-wifi = "{essid} {icon}${u "\\ue621"} {bandwidthUpBits}${u "\\ue621"} {bandwidthDownBytes} ${u "\\udb82\\udfce"} ";
            tooltip-format-ethernet = "{bandwidthUpBits}${u "\\ue621"} {bandwidthDownBytes} ${u "\\udb82\\udfce"} ";
            tooltip-format-disconnected = "Disconnected";
            on-click = "gtk-launch nm-connection-editor.desktop";
          };
          bluetooth = {
            format = "{status} ${u "\\uf294"}";
            format-disabled = "";
            format-connected = "{device_alias} ${u "\\uf294"}";
            format-connected-battery = "{device_alias} - {device_battery_percentage}%${u "\\uf294"} ";
            tooltip-format = "{controller_alias}\t{controller_address}\n\n{num_connections} connected";
            tooltip-format-connected = "{controller_alias}\t{controller_address}\n\n{num_connections} connected\n\n{device_enumerate}";
            tooltip-format-enumerate-connected = "{device_alias}\t{device_address}";
            tooltip-format-enumerate-connected-battery = "{device_alias}\t{device_address}\t{device_battery_percentage}%";
            on-click = "bluedevil-wizard";
          };
          # KEYBOARD -------- ---- -- -
          keyboard-state = {
            numlock = true;
            capslock = true;
            scrolllock = true;
            format = {
              numlock = "N {icon}";
              capslock = "C {icon}";
              scrolllock = "S {icon}";
            };
            format-icons = {
              locked = u "\\uf023";
              unlocked = u "\\uf13e";
            };
          };
          # GROUP-MEDIA -------- ---- -- -
          "group/media" = {
            orientation = "horizontal";
            modules = [
              # TODO: add mpris/mpd
              "wireplumber"
            ];
            drawer.transition-left-to-right = false;
          };
          wireplumber = {
            tooltip = true;
            format = "{volume}%  {icon}";
            format-muted = "<s>{volume}%</s> ${u "\\uf026"}";
            format-icons = [
              (u "\\uf026")
              (u "\\uf027")
              (u "\\uf027")
              (u "\\uf028")
            ];
            on-click = "pamixer -t";
            on-click-right = "gtk-launch pavucontrol-qt.desktop";
            max-volume = 100;
            scroll-step = 5;
          };
          # DATE/TIME -------- ---- -- -
          "clock#date" = {
            format = "{:%a %d %b}  ${u "\\uf073"}";
            tooltip = true;
            tooltip-format = "<tt>{calendar}</tt>";
            # TODO: calendar stopped working
            calendar = {
              mode = "month";
              weeks-pos = "";
              mode-mon-col = 3;
              on-scroll = 1;
              on-click-right = "mode";
              format = {
                months = "<span color='${foreground}'><b>{}</b></span>";
                days = "<span color='${white}'>{}</span>";
                weekdays = "<span color='${white}'><b>{}</b></span>";
                today = "<span color='${foreground}'><b>{}</b></span>";
              };
            };
            actions = {
              on-click = "mode";
              on-scroll-up = "shift_up";
              on-scroll-down = "shift_down";
            };
          };
          "clock#time" = {
            interval = 1;
            format = "{:%H:%M:%S}  ${u "\\uf017"}";

            # TODO: tooltip timezone
            tooltip = false;
          };
        };
      };
      style = withVars theme (builtins.readFile ./waybar/style.scss);
    };

  programs.kitty = {
    enable = true;
    shellIntegration.enableFishIntegration = true;
    settings = {
      scrollback_lines = 1000000;
      startup_session = "default.session";
    };
    extraConfig = with themes.popping-and-locking-black; ''
      shell fish
      editor micro

      background_opacity 0.75

      background  ${background}
      foreground  ${foreground}

      cursor                ${cursorColor}
      selection_foreground  none
      selection_background  ${selectionBackground}

      color0  ${black}
      color8  ${brightBlack}
      color1  ${red}
      color9  ${brightRed}
      color2  ${green}
      color10 ${brightGreen}
      color3  ${yellow}
      color11 ${brightYellow}
      color4  ${blue}
      color12 ${brightBlue}
      color5  ${purple}
      color13 ${brightPurple}
      color6  ${cyan}
      color14 ${brightCyan}
      color7  ${white}
      color15 ${brightWhite}
    '';
  };

  xdg = {
    enable = true;
    configFile = {
      "kitty/default.session".text = ''
        cd ~/Desktop
      '';
      "hypr/hyprpaper.conf".text = ''
        preload = ${./wallpaper/background.png}
        wallpaper = ,${./wallpaper/background.png}
      '';
    };
  };

  qt = {
    enable = true;
    platformTheme = "gtk";
    style.name = "breeze";
  };


  programs.bash = {
    sessionVariables = {
      #XDG_CONFIG_HOME = "$HOME/.config";
      #XDG_DATA_HOME = "$HOME/.local/share";
      #XDG_CACHE_HOME = "$HOME/.cache";
    };
  };

  #programs.waybar = {
  #  enable = true;
  #  settings = {
  #    mainBar = {
  #      layer = "top";
  #      position = "left";
  #      width = 32;
  #      output = [
  #        "HDMI-A-1"
  #      ];
  #      modules-left = [
  #      ];
  #      modules-center = [];
  #      modules-right = [];
  #      margin = 16;
  #    };
  #  };
  #};

  # TODO: configure
  services.dunst = {
    # TODO: avizo or dunstify as volume/brightness level indicator or 
    enable = true;
    iconTheme = {
      name = "Adwaita";
      package = pkgs.gnome.adwaita-icon-theme;
      size = "32x32";
    };
    settings = {
      global = {
        width = 300;
        height = 300;
        offset = "30x50";
        origin = "top-right";
        transparency = 10;
        frame_color = "#eceff1";
        font = "Droid Sans 9";
      };

      urgency_normal = {
        background = "#37474f";
        foreground = "#eceff1";
        timeout = 10;
      };
    };
  };
}
