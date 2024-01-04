{ config, inputs, lib, pkgs, ... }:
let
  enable = config.preset.development;
in
{
  options.preset.development = lib.mkEnableOption "Enable development preset";

  config = (lib.mkIf enable {
    module = {
      shell.enable = true;
      agenix.enable = true;
      copyFile."Desktop/work/flake.nix" = ../../assets/devenv/flake.nix;
    };

    home.file."Desktop/work/.envrc".source = ../../assets/devenv/.envrc;

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
