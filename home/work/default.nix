{ config, lib, pkgs, specialArgs, ... }:
with specialArgs;
let
  envrc = "Desktop/work/.envrc";
  devenv = "Desktop/work/devenv";
  copy = orig: dest: # TODO: extract this functionality to a module
    lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      $DRY_RUN_CMD cp -u ${builtins.toPath orig} ${dest}
      $DRY_RUN_CMD chmod 400 ${dest}
    '';
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
    "${envrc}".source = ../../assets/.envrc;
    "${devenv}.yaml".source = ./devenv.yaml;
  };

  programs.direnv.config = {
    whitelist.exact = [ "${config.home.homeDirectory}/${envrc}" ];
  };

  home.activation = {
    paackWorkFlake = copy ./devenv.nix "${devenv}.nix";
  };
}
