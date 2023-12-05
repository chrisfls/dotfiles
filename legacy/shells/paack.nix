{ pkgs ? import <nixpkgs> { } }:
with pkgs;
mkShell {
  buildInputs = with pkgs; [
    elmPackages.elm-format
    elmPackages.elm-language-server
    nodejs-16_x # vscode's nodejs version for elm lsp
    yarn
    python39 # needed by pack
    google-cloud-sdk # needed by monorepo
  ];
}
