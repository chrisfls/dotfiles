{ config, lib, pkgs, ... }:
let
  inherit (config.modules.feh) enable wallpapers;

  args = lib.trivial.pipe wallpapers [
    (map (path: "\"${path}\""))
    (builtins.concatStringsSep " ")
  ];
in
{
  options.modules.feh = {
    enable = lib.mkEnableOption "Enable feh module";
    wallpapers = lib.mkOption {
      type = lib.types.listOf lib.types.path;
      default = { };
    };
  };

  config = lib.mkIf enable {
    pacman.packages = [ "extra/feh" ];

    modules.i3wm.startup = [ "feh --bg-center ${args}" ];
  };
}
