{ config, inputs, pkgs, ... }:
{
  imports = [
    ../shell.nix
    ../agenix.nix
  ];

  config = {
    home.packages = [
      # nix-shell / nix flake develop
      pkgs.any-nix-shell
      pkgs.which

      # devenv
      pkgs.cachix
      inputs.devenv.packages.${pkgs.system}.devenv

      # misc
      pkgs.nixpkgs-fmt
    ];

    programs.fish.shellInit = "any-nix-shell fish --info-right | source";

    programs.direnv.config.whitelist.prefix = [ "${config.xdg.userDirs.desktop}" ];

    programs.nix-index.enable = true;
    programs.nix-index.enableFishIntegration = true;
  };
}
