{ config, lib, pkgs, specialArgs, ... }:
{
  nixpkgs.config.allowUnfree = true;
  home.packages = with pkgs; [
    krita
    steam
    steam-tui
  ];
}
