{ config, lib, pkgs, ... }:
let inherit (config.modules.podman) enable; in {
  options.modules.podman.enable = lib.mkEnableOption "Enable podman module";

  config = lib.mkIf enable {
    pacman.packages = [ "extra/podman" "extra/docker-compose" "extra/cni-plugins" ];

    home.sessionVariables.DOCKER_HOST = "unix://$XDG_RUNTIME_DIR/podman/podman.sock";
  };
}
