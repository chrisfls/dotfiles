{ config, lib, pkgs, ... }:
let inherit (config.module.podman) enable; in {
  options.module.podman.enable = lib.mkEnableOption "Enable podman module";

  config = lib.mkIf enable {
    pacman.packages = [ "extra/podman" "extra/docker-compose" ];

    home.sessionVariables.DOCKER_HOST = "unix://$XDG_RUNTIME_DIR/podman/podman.sock";
  };
}
