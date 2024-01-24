# REVIEW: rename archlinux
{ config, lib, pkgs, ... }:
let
  enable = config.presets.non-nixos;
in
{
  options.presets.non-nixos = lib.mkEnableOption "Enable non-nixos preset";

  config = (lib.mkIf enable {
    targets.genericLinux.enable = true;
    
    modules.xorg.imported-variables  = [ "PATH" ];
  });
}
