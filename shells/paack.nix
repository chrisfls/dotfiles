{ pkgs ? import <nixpkgs> { } }:
with pkgs;
mkShell {
  buildInputs = with pkgs; [
    elmPackages.elm-format
    nodejs
    yarn
    python39 # needed by pack
  ];
}
