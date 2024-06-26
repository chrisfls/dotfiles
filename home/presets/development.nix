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
    };

    pacman.packages = [
      "extra/deno"
      "extra/ollama"
    ];

    # nix stuff
    home.packages = [
      pkgs.devenv
      pkgs.cachix
      pkgs.nixpkgs-fmt
    ];
  };
}
