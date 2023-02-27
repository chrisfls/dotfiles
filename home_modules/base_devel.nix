# base for development machines
{ pkgs, ... }:
{
  imports = [./base.nix];
  home.packages = with pkgs; [
    any-nix-shell
    nixpkgs-fmt

    # languagetool
    adoptopenjdk-jre-openj9-bin-16
    ltex-ls

    # used just to setup cloudflare warp
    cloudflare-warp
    xdg-utils
    desktop-file-utils
  ];
}