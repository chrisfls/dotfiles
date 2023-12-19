{ config, lib, pkgs, specialArgs, ... }:
let
  cfg = config.extra.polybar;

  scripts = pkgs.fetchFromGitHub {
    owner = "polybar";
    repo = "polybar-scripts";
    rev = "879834652947a7c6af894effc577dd2bfaa920b7";
    sha256 = "sha256-bhuS2SQYP2KvwkOwfdjGLlismmNyDiViOOkIbVJPR+c=";
  };
in
{
  options.extra.polybar = {
    enable = lib.mkEnableOption "Enable polybar module";
    color-scheme = lib.mkOption {
      type = lib.types.attrs;
      default = specialArgs.color-schemes.popping-and-locking-black;
    };
  };

  config = lib.mkIf cfg.enable {
    services.polybar = {
      enable = true;
      script = "polybar -r &";
      settings = {};
    };

    xdg.configFile = {
      "polybar/pipewire-simple.sh" = {
        source = ./polybar/pipewire-simple.sh;
        executable = true;
      };
    };
  };
}

