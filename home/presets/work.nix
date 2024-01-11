{ config, lib, specialArgs, ... }:
let
  enable = config.presets.work;
  ssot = specialArgs.ssot;
in
{
  options.presets.work = lib.mkEnableOption "Enable work preset";

  config = (lib.mkIf enable {
    programs.git.includes = [
      {
        condition = "gitdir:${config.xdg.userDirs.desktop}/work/";
        contents = {
          user.name = ssot.contact.work.name;
          user.email = ssot.contact.work.email;
        };
      }
    ];
  });
}
