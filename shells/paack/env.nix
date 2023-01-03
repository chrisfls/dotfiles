{ pkgs ? import <nixpkgs> { }, lib ? pkgs.lib, getDefaultPaths ? lib.trivial.const [ ], ... }@attrs:
pkgs.buildEnv rec {
  name = "paack";
  paths = with pkgs; (getDefaultPaths attrs) ++ [
    elmPackages.elm-format
    nodejs
    yarn
  ];
  pathsToLink = [ "/bin" ];
}
