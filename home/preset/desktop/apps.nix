{ config, lib, pkgs, specialArgs, ... }:
let
  lxqt = pkgs.lxqt;

  cfg = config.extra.shell;

  defaultCmd = { packages, ... }: builtins.baseNameOf (lib.getExe (builtins.head packages));
in
{
  options.extra.shell = {
    xdg-desktop-portal = {
      enable = lib.mkEnableOption "Enable xdg-desktop-portal";

      cmd = lib.mkOption {
        type = lib.types.str;
        default = defaultCmd cfg.xdg-desktop-portal;
      };

      packages = lib.mkOption {
        type = lib.types.listOf lib.types.package;
        default = [ lxqt.xdg-desktop-portal-lxqt ];
      };
    };

    notifications = {
      enable = lib.mkEnableOption "Enable notifications";

      cmd = lib.mkOption {
        type = lib.types.str;
        default = defaultCmd cfg.notifications;
      };

      packages = lib.mkOption {
        type = lib.types.listOf lib.types.package;
        default = [ lxqt.lxqt-notificationd ];
      };
    };

    polkit-agent = {
      enable = lib.mkEnableOption "Enable polkit-agent";

      cmd = lib.mkOption {
        type = lib.types.str;
        default = defaultCmd cfg.polkit-agent;
      };

      packages = lib.mkOption {
        type = lib.types.listOf lib.types.package;
        default = [ lxqt.lxqt-policykit ];
      };
    };

    gui-sudo = {
      enable = lib.mkEnableOption "Enable gui-sudo";

      cmd = lib.mkOption {
        type = lib.types.str;
        default = defaultCmd cfg.gui-sudo;
      };

      packages = lib.mkOption {
        type = lib.types.listOf lib.types.package;
        default = [ lxqt.lxqt-sudo ];
      };
    };

    ssh-askpass = {
      enable = lib.mkEnableOption "Enable ssh-askpass";

      cmd = lib.mkOption {
        type = lib.types.str;
        default = defaultCmd cfg.ssh-askpass;
      };

      packages = lib.mkOption {
        type = lib.types.listOf lib.types.package;
        default = [ lxqt.lxqt-openssh-askpass ];
      };
    };

    file-manager = {
      enable = lib.mkEnableOption "Enable file-manager";

      cmd = lib.mkOption {
        type = lib.types.str;
        default = defaultCmd cfg.file-manager;
      };

      packages = lib.mkOption {
        type = lib.types.listOf lib.types.package;
        default = [ lxqt.pcmanfm-qt ];
      };
    };

    volume-mixer = {
      enable = lib.mkEnableOption "Enable volume-mixer";

      cmd = lib.mkOption {
        type = lib.types.str;
        default = defaultCmd cfg.volume-mixer;
      };

      packages = lib.mkOption {
        type = lib.types.listOf lib.types.package;
        default = [ lxqt.pavucontrol-qt ];
      };
    };

    system-monitor = {
      enable = lib.mkEnableOption "Enable system-monitor";

      cmd = lib.mkOption {
        type = lib.types.str;
        default = defaultCmd cfg.system-monitor;
      };

      packages = lib.mkOption {
        type = lib.types.listOf lib.types.package;
        default = [ lxqt.qps ];
      };
    };

    clipboard-manager = {
      enable = lib.mkEnableOption "Enable clipboard-manager";

      cmd = lib.mkOption {
        type = lib.types.str;
        default = defaultCmd cfg.clipboard-manager;
      };

      packages = lib.mkOption {
        type = lib.types.listOf lib.types.package;
        default = [ lxqt.qlipper ];
      };
    };

    screenshot = {
      enable = lib.mkEnableOption "Enable screenshot";

      cmd = lib.mkOption {
        type = lib.types.str;
        default = "shotgun - | xclip -t 'image/png' -selection clipboard";
      };

      packages = lib.mkOption {
        type = lib.types.listOf lib.types.package;
        default = [ pkgs.shotgun pkgs.xclip ];
      };
    };

    screenshot-alt = {
      enable = lib.mkEnableOption "Enable screenshot-alt";

      cmd = lib.mkOption {
        type = lib.types.str;
        default = "shotgun -g \"$(hacksaw -s 2)\" - | xclip -t 'image/png' -selection clipboard";
      };

      packages = lib.mkOption {
        type = lib.types.listOf lib.types.package;
        default = [ pkgs.shotgun pkgs.hacksaw pkgs.xclip ];
      };
    };

    browser = {
      enable = lib.mkEnableOption "Enable browser";

      cmd = lib.mkOption {
        type = lib.types.str;
        default = "brave";
      };

      package = lib.mkOption {
        type = lib.types.package;
        default = pkgs.brave;
      };
    };

    terminal = {
      enable = lib.mkEnableOption "Enable terminal";

      cmd = lib.mkOption {
        type = lib.types.str;
        default = "contour";
      };

      package = lib.mkOption {
        type = lib.types.package;
        default = pkgs.contour;
      };
    };

    launcher = {
      enable = lib.mkEnableOption "Enable launcher";

      cmd = lib.mkOption {
        type = lib.types.str;
        default = "rofi -show drun";
      };

      package = lib.mkOption {
        type = lib.types.package;
        default = pkgs.rofi;
      };
    };
  };

  config = lib.mkMerge [
    {
      extra.nixGL.overlay = {
        lxqt = {
          lxqt-sudo = [ "lxqt-sudo" ];
          pavucontrol-qt = [ "pavucontrol-qt" ];
          pcmanfm-qt = [ "pcmanfm-qt" ];
          qlipper = [ "qlipper" ];
          qps = [ "qps" ];
        };
        brave = [ "brave" ];
        contour = [ "contour" ];
      };
    }
    (with cfg.xdg-desktop-portal; lib.mkIf enable { home.packages = packages; })
    (with cfg.notifications; lib.mkIf enable { home.packages = packages; })
    (with cfg.polkit-agent; lib.mkIf enable { home.packages = packages; })
    (with cfg.gui-sudo; lib.mkIf enable { home.packages = packages; })
    (with cfg.ssh-askpass; lib.mkIf enable { home.packages = packages; })
    (with cfg.file-manager; lib.mkIf enable { home.packages = packages; })
    (with cfg.volume-mixer; lib.mkIf enable { home.packages = packages; })
    (with cfg.system-monitor; lib.mkIf enable { home.packages = packages; })
    (with cfg.clipboard-manager; lib.mkIf enable { home.packages = packages; })
    (with cfg.screenshot; lib.mkIf enable { home.packages = packages; })
    (with cfg.screenshot-alt; lib.mkIf enable { home.packages = packages; })
    (with cfg.terminal; lib.mkIf enable { home.packages = [package]; })
    (with cfg.browser; lib.mkIf enable { home.packages = [package]; })
  ];
}
