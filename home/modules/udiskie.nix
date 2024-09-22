# REVIEW: not sure this is needed anymore, plasma has its own thing
{ config, lib, pkgs, specialArgs, ... }:
let inherit (config.modules.udiskie) enable; in
{
  options.modules.udiskie.enable = lib.mkEnableOption "Enable udiskie module";

  config = lib.mkIf enable {
    pacman.packages = [ "extra/udisks2" "extra/udiskie" ];

    xdg.configFile."udiskie/config.yml".text =
      ''
        program_options:
          automount: true
          notify: true
          tray: auto
      '';

    # REVIEW: see if this can be moved to plasma
    systemd.user.services.udiskie = {
      Unit = {
        Description = "udiskie mount daemon";
        Requires = "tray.target";
        After = [ "graphical-session-pre.target" "tray.target" ];
        PartOf = [ "graphical-session.target" ];
      };

      Service.ExecStart = "/usr/bin/udiskie";

      Install.WantedBy = [ "graphical-session.target" ];
    };
  };
}
