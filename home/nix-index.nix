{ config, pkgs, lib, ... }:
with lib;
let
  cfg = config.module.nix-index;
in
{
  options.module.nix-index = {
    enable = mkEnableOption "nix-index module";
  };

  config = mkIf cfg.enable {
    programs.nix-index.enable = true;
  };
}
