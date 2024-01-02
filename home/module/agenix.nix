{ config, inputs, lib, pkgs, ... }:
let inherit (config.module.agenix) enable; in {
  options.module.agenix.enable = lib.mkEnableOption "Enable agenix module";

  imports = [ inputs.homeage.homeManagerModules.homeage ];

  config.home.packages = lib.mkIf enable [ inputs.agenix.packages.${pkgs.system}.default ];
}
