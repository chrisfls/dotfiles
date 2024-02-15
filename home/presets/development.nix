{ config, inputs, lib, pkgs, ... }:
let
  enable = config.presets.development;
in
{
  options.presets.development = lib.mkEnableOption "Enable development preset";

  config = (lib.mkIf enable {
    modules = {
      agenix.enable = true;
      podman.enable = true;
    };

    home.packages = [
      # devenv
      pkgs.cachix

      # misc
      pkgs.nixpkgs-fmt
    ];

    programs = {
      # nix-index = {
      #   enable = true;
      #   enableFishIntegration = true;
      # };
    };
  });
}
