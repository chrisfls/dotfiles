{ pkgs, ... }:
{
  imports = [
    ./development.nix
  ];

  config = {
    home.packages = [
      pkgs.aseprite
      pkgs.krita
      pkgs.tiled
      pkgs.lmms
    ];

    nixpkgs.config.allowUnfree = true;
  };
}
