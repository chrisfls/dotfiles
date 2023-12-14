{ inputs, pkgs, ... }:
{
  imports = [
    inputs.homeage.homeManagerModules.homeage
  ];

  config.home.packages = [
    inputs.agenix.packages.${pkgs.system}.default
  ];
}
