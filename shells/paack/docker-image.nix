#! /usr/bin/env nix-build
{ pkgs ? import <nixpkgs> { }, ... }:
# still doesn't work
let
  attrs = {
    inherit pkgs;
    lib = pkgs.lib;
    getDefaultPaths = import ../get-default-docker-paths.nix;
  };
in
pkgs.dockerTools.buildImage {
  name = "paack-env";
  tag = "latest";
  copyToRoot = import ./env.nix attrs;
}
