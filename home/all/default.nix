{ config, pkgs, specialArgs, ... }:
with specialArgs;
{
  imports = [ homeage.homeManagerModules.homeage ];

  homeage = {
    identityPaths = [ ".ssh/id_ed25519" ];
    installationType = "systemd";
  };
}

