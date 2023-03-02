{ config, pkgs, lib, ... }:
with lib;
let
  cfg = config.module.direnv;
  bin = "${pkgs.direnv}/bin/direnv";
  direnvAllow = (path: "$DRY_RUN_CMD sh -c 'if [ -f \"${path}/.envrc\" ]; then ${bin} allow \"${path}\"; fi;'");
in
{
  imports = [
    ./nix-index.nix
  ];

  options.module.direnv = {
    enable = mkEnableOption "direnv module";
  };

  config = mkIf cfg.enable {
    module.nix-index.enable = true;

    programs.direnv = {
      enable = true;
      nix-direnv.enable = true;
    };
    
    home.activation = {
      direnvAllow = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        ${direnvAllow "$HOME"}
        ${direnvAllow "$HOME/paack"}
      '';
    };
  };
}
