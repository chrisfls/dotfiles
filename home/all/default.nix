{ config, flakes, pkgs, ... }:
{
  imports = [ flakes.homeage.homeManagerModules.homeage ];

  homeage = {
    identityPaths = [ ".ssh/id_ed25519" ];
    installationType = "systemd";
  };
}

