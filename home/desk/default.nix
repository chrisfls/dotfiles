{ config, pkgs, lib, specialArgs, ... }:
{
  home.packages = with pkgs; [
    languagetool
  ];
}
