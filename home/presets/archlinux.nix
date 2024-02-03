# REVIEW: rename archlinux
{ config, lib, pkgs, ... }:
let
  enable = config.presets.archlinux;
in
{
  options.presets.archlinux = lib.mkEnableOption "Enable archlinux preset";

  config = (lib.mkIf enable {
    targets.genericLinux.enable = true;
    
    modules.xorg.imported-variables  = [ "PATH" ];
  });
}
