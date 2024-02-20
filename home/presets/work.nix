{ config, lib, specialArgs, ... }:
let
  enable = config.presets.work;
  ssot = specialArgs.ssot;
in
{
  options.presets.work = lib.mkEnableOption "Enable work preset";

  config = (lib.mkIf enable {
    modules.git.extraConfig =
      ''
        [includeIf "gitdir:${config.xdg.userDirs.desktop}/work/"]
        	path = "${config.xdg.configHome}/git/config_work"
      '';

    xdg.configFile."git/config_work".text =
      ''
        [user]
        	email = "${ssot.contact.work.email}"
        	name = "${ssot.contact.work.name}"
      '';
  });
}
