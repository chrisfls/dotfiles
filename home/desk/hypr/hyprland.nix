{ config, lib, specialArgs, ... }:
with specialArgs;
{
  config = {
    xdg.configFile."hypr/hyprland.conf".text = my.replaceVars
      (lib.mapAttrs (name: hex: builtins.substring 1 (-1) hex) config.theme)
      (builtins.readFile ./hyprland.conf);

    # TODO:
    /*
      xdg.configFile."hypr/hyprpaper.conf".text = ''
      preload = ${./wallpaper/background.png}
      wallpaper = ,${./wallpaper/background.png}
      '';
    */
  };
}
