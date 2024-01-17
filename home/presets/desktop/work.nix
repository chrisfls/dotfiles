{ config, lib, pkgs, specialArgs, ... }:
let
  inherit (config.presets) desktop work;
  enable = desktop && work;
in
{
  config = lib.mkIf enable {
    modules.microsoft-edge.enable = true;
  };
}
