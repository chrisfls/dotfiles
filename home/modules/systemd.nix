# WAYLAND_DISPLAY
{ config, lib, pkgs, specialArgs, ... }:
let inherit (config.modules.systemd) enable imported-variables; in {
  options.modules.systemd = {
    enable = lib.mkEnableOption "Enable systemd module";
    imported-variables = lib.mkOption { type = lib.types.listOf lib.types.str; default = [ ]; };
    variables = lib.mkOption {
      type = lib.types.str;
      default = lib.trivial.pipe imported-variables [
        (builtins.map (name: "'${name}'"))
        (builtins.concatStringsSep " ")
      ];
    };
  };
}
