{ config, inputs, lib, pkgs, ... }:
let
  cfg = config.extra.agenix;
in
{
  options.extra.agenix.enable = lib.mkEnableOption "Enable agenix module";

  imports = [ inputs.homeage.homeManagerModules.homeage ];

  config = lib.mkIf cfg.enable {
    home.packages = [ inputs.agenix.packages.${pkgs.system}.default ];
  };
}
