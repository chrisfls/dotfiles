{ config, lib, pkgs, specialArgs, ... }:
let
  inherit (config.presets) non-nixos;
  inherit (config.modules.udiskie) enable;
in
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

    systemd.user.services.udiskie = {
      Unit = {
        Description = "udiskie mount daemon";
        Requires = "tray.target";
        After = [ "graphical-session-pre.target" "tray.target" ];
        PartOf = [ "graphical-session.target" ];
      };

      Service.ExecStart =
        if non-nixos then "/usr/bin/udiskie"
        else "${pkgs.udiskie}/bin/udiskie";

      Install.WantedBy = [ "graphical-session.target" ];
    };
  };
}
