{ config, inputs, lib, pkgs, ... }:
let
  inherit (config.presets) desktop;
  enable = config.presets.development;
in
{
  options.presets.development = lib.mkEnableOption "Enable development preset";

  config = lib.mkIf enable {
    presets.desktop = true;

    modules = {
      agenix.enable = true;
      podman.enable = true;
      helix.enable = true;
      code.enable = true;
      # TODO: sublime
    };

    # pacman.packages = ["chaotic-aur/gittyup" "chaotic-aur/github-desktop"];

    home.packages = [
      # devenv
      inputs.devenv.packages.${pkgs.system}.devenv
      pkgs.cachix

      # misc
      pkgs.nixpkgs-fmt
    ];

    # programs.nix-index = {
    #   enable = true;
    #   enableFishIntegration = true;
    # };
  };
}
