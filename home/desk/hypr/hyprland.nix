{ config, lib, specialArgs, ... }:
with specialArgs;
{
  options.theme = lib.mkOption { };

  config = {
    xdg.configFile."hypr/hyprland.conf".text =
      let
        theme = lib.mapAttrs
          (name: hex: builtins.substring 1 (-1) hex)
          config.theme;
      in
      replaceVars theme (builtins.readFile ./hyprland.conf);

    /*
      xdg.configFile."hypr/hyprpaper.conf".text = ''
      preload = ${./wallpaper/background.png}
      wallpaper = ,${./wallpaper/background.png}
      '';
    */
  };
}
