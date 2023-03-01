{ config, pkgs, lib, specialArgs, ... }:
with lib;
with specialArgs;
let
  cfg = config.module.homeage;
in
{
  options.module.homeage = {
    enable = mkEnableOption "homeage module";
    username = mkOption { type = types.str; };
    identities = mkOption { type = types.listOf types.str; };
    file = lib.mkOption {
      type = lib.types.attrsOf lib.types.string;
      default = { };
    };
  };

  config = mkIf cfg.enable {
    homeage = {
      identityPaths = map (path: "/home/${config.module.home.username}/${path}") cfg.identities;
      installationType = "activation";
      file = lib.attrsets.mapAttrs'
        (target: source:
          {
            name = toString source;
            value = {
              source = builtins.toPath "${nix-secrets}/users/${nixosConfig.networking.hostName}/${config.module.home.username}/${source}";
              copies = [ "/home/${config.module.home.username}/${target}" ];
            };
          }
        )
        cfg.file;
    };
  };
}
