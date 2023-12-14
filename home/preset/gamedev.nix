{ pkgs, ... }:
{
  imports = [
    ./development.nix
  ];

  config = {
    home.packages = with pkgs; [
      aseprite
      krita
      tiled
      lmms
    ];

    nixpkgs.config.allowUnfree = true;
  };
}
