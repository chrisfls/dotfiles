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
      shell.enable = true;
    };

    home.packages = [
      # nix-shell / nix flake develop
      pkgs.any-nix-shell

      # devenv
      pkgs.cachix

      # misc
      pkgs.nixpkgs-fmt
    ];

    programs = {
      fish.shellInit = "any-nix-shell fish --info-right | source";

      direnv.config.whitelist.prefix = [ "${config.xdg.userDirs.desktop}" ];

      # nix-index = {
      #   enable = true;
      #   enableFishIntegration = true;
      # };
    };
  });
}