{ pkgs ? import <nixpkgs> { } }:
with pkgs;
mkShell {
  buildInputs = with pkgs; [
    elmPackages.elm-format
    nodejs-16_x # vscode's nodejs version for elm lsp
    yarn
    python39 # needed by pack
  ];
}
 