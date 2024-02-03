{ config, lib, pkgs, ... }:
let
  inherit (config.presets) archlinux;
  inherit (config.modules.autorandr) enable;

  autorandr =
    if archlinux then "/usr/bin/autorandr"
    else "${pkgs.autorandr}/bin/autorandr";
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
