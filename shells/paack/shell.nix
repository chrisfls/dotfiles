#! /usr/bin/env nix-shell
{ pkgs ? import <nixpkgs> { }, lib ? pkgs.lib, ... }@attrs:
pkgs.mkShell rec {
  name = "paack-dev";
  buildInputs = [
    (import ./env.nix attrs)
  ];
  shellHook = pkgs.writeShellScript name ''
  '';
}
