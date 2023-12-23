{ config, lib, specialArgs, ... }:
let
  enable = config.preset.work;
  ssot = specialArgs.ssot;
in
{
  options.preset.work = lib.mkEnableOption "Enable work preset";

  config = (lib.mkIf enable {
    programs.git = {
      enable = true;
      includes = [
        {
          condition = "gitdir:${config.xdg.userDirs.desktop}/work/";
          contents = {
            user.name = ssot.contact.work.name;
            user.email = ssot.contact.work.email;
          };
        }
      ];
    };
  });
}
