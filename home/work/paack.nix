{ config, lib, pkgs, specialArgs, ... }:
with specialArgs;
let
  envrc = "Desktop/work/.envrc";
in
{
  programs.git = {
    includes = [
      {
        condition = "gitdir:${config.home.homeDirectory}/work/";
        contents = {
          user.name = ssot.contact.work.name;
          user.email = ssot.contact.work.email;
        };
      }
    ];
  };

  home.file = {
    "${envrc}".source = ../../devenv/.envrc;
  };

  homeage.file = {
    secret-envrc = {
      source = ./dot-envrc.secret.age;
      symlinks = [ "${envrc}.secret" ];
    };
  };

  programs.direnv.config = {
    whitelist.exact = [ "${config.home.homeDirectory}/${envrc}" ];
  };

  home.activation =
    let
      direnv = "${pkgs.direnv}/bin/direnv";
      orig = builtins.toPath ../../devenv/paack.nix;
      dest = "Desktop/work/flake.nix";
    in
    {
      workFlake = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        $DRY_RUN_CMD cp -f ${orig} ${dest}
        $DRY_RUN_CMD chmod 400 ${dest}
      '';
    };
}
