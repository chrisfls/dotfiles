{ config, inputs, lib, pkgs, ... }:
let
  inherit (config.presets) desktop;
  enable = config.presets.development;
in
{
  options.presets.development = lib.mkEnableOption "Enable development preset";

  config = lib.mkIf enable {
    presets.desktop = true;

    modules = {
      agenix.enable = true;
      podman.enable = true;
      helix.enable = true;
      code.enable = true;
      asdf.enable = true;
    };

    pacman.packages = [
      # mine
      "extra/deno"
      "extra/ollama"
      # timeline
      "chaotic-aur/heroku-cli-bin"
      "chaotic-aur/ngrok"
      "chaotic-aur/postman-bin"
      "core/ncurses"
      "core/unixodbc"
      "extra/chromium"
      "extra/fop"
      "extra/inotify-tools"
      "extra/libssh"
      "extra/libxslt"
      "extra/postgresql"
      "extra/wxwidgets-gtk3"
    ];

    # nix stuff
    home.packages = [
      pkgs.devenv
      pkgs.cachix
      pkgs.nixpkgs-fmt
    ];
  };
}
