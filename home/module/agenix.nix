{ config, inputs, lib, pkgs, ... }:
let
  cfg = config.module.agenix;
in
{
  options.module.agenix.enable = lib.mkEnableOption "Enable agenix module";

  imports = [ inputs.homeage.homeManagerModules.homeage ];

  config = lib.mkIf cfg.enable {
    home.packages = [ inputs.agenix.packages.${pkgs.system}.default ];
  };
}
