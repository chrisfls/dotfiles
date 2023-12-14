{ config, inputs, pkgs, ... }:
{
  imports = [
    ../console.nix
    ../agenix.nix
    ../codium.nix
  ];

  config = {
    home.packages = with pkgs; [
      # nix-shell / nix flake develop
      any-nix-shell
      which

      # devenv
      cachix
      inputs.devenv.packages.${pkgs.system}.devenv

      # misc
      nixpkgs-fmt
    ];

    programs.fish.shellInit = "any-nix-shell fish --info-right | source";

    programs.direnv.config.whitelist.prefix = [ "${config.xdg.userDirs.desktop}" ];

    programs.nix-index.enable = true;
    programs.nix-index.enableFishIntegration = true;
  };
}
