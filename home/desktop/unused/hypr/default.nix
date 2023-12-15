{ config, lib, specialArgs, ... }:
with specialArgs;
{
  config = {
    xdg.configFile."hypr/hyprland.conf".text = my.replaceVars
      (lib.mapAttrs
        (name: hex: builtins.substring 1 (-1) hex)
        config.themes.popping-and-locking)
      (builtins.readFile ./hyprland.conf);
  };
}
