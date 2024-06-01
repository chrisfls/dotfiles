{ config, lib, specialArgs, ... }:
let
  enable = config.presets.work;
  ssot = specialArgs.ssot;
in
{
  options.presets.work = lib.mkEnableOption "Enable work preset";

  config = lib.mkIf enable {
    presets.desktop = true;

    pacman.packages = [
      "extra/dbeaver"
      # vivaldi and firefox are the least buggy browsers on wayland now
      # firefox does not have 
      "extra/vivaldi"
      "extra/vivaldi-ffmpeg-codecs"
    ];

    modules = {
      git.extraConfig =
        ''
          [includeIf "gitdir:${config.xdg.userDirs.desktop}/work/"]
          	path = "${config.xdg.configHome}/git/config_work"
        '';
    };

    xdg.configFile."git/config_work".text =
      ''
        [user]
        	name = "${ssot.contact.work.name}"
        	email = "${ssot.contact.work.email}"
      '';
  };
}
