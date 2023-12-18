{ config, specialArgs, ... }:
let
  ssot = specialArgs.ssot;
in
{
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
}
