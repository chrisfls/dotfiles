{ pkgs, ... }:
{
  packages = with pkgs; [
    elmPackages.elm-format
    elmPackages.elm-language-server
    nodejs-16_x # vscode's nodejs version for elm lsp
    yarn
    python39
    google-cloud-sdk # needed for monorepo
  ];
}
