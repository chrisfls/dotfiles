{ config, lib, ... }:
let
  enable = config.preset.non-nixos;
in
{
  options.preset.non-nixos = lib.mkEnableOption "Enable non-nixos preset";

  config = (lib.mkIf enable {
    targets.genericLinux.enable = true;
  });
}
