{ config, inputs, lib, pkgs, ... }:
let inherit (config.modules.agenix) enable; in {
  options.modules.agenix.enable = lib.mkEnableOption "Enable agenix module";

  imports = [ inputs.homeage.homeManagerModules.homeage ];

  config.home.packages = lib.mkIf enable [ inputs.agenix.packages.${pkgs.system}.default ];
}
