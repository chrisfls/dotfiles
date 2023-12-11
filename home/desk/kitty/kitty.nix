{ config, lib, specialArgs, ... }:
with specialArgs;
{
  config = {
    # TODO: probably stop using home manager (why even bother?)
    programs.kitty = {
      enable = true;
      shellIntegration.enableFishIntegration = true;
      settings = {
        scrollback_lines = 1000000;
        startup_session = "default.session";
      };
      extraConfig = my.replaceVars config.themes.popping-and-locking-black (builtins.readFile ./kitty.conf);
    };

    xdg.configFile."kitty/default.session".text = "cd ~/Desktop";
  };
}
