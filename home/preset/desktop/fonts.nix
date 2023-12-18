{ pkgs, ... }:
{
  home.packages = [
    # good heading fonts
    pkgs.overpass
    pkgs.montserrat

    # terminal fonts
    # pkgs.nerdfonts

    # programming fonts
    pkgs.jetbrains-mono
    pkgs.cascadia-code
  ];
}
