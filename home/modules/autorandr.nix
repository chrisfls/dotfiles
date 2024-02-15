{ config, lib, pkgs, ... }:
let
  inherit (config.modules.autorandr) enable;

  autorandr = "/usr/bin/autorandr";
in
{
  options.modules.autorandr.enable = lib.mkEnableOption "Enable autorandr module";

  config = lib.mkIf enable {
    pacman.packages = [ "extra/autorandr" ];

    modules.xorg.xsession = "${autorandr} -c";

    systemd.user.services.autorandr = {
      Unit = {
        Description = "autorandr";
        After = [ "graphical-session-pre.target" ];
        PartOf = [ "graphical-session.target" ];
      };

      Service = {
        Type = "oneshot";
        ExecStart = "${autorandr} --change";
      };

      Install.WantedBy = [ "graphical-session.target" ];
    };
  };
}
