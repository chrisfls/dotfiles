# put dash scripts in the tmp and call them with $SCRIPT/name
{ config, lib, pkgs, ... }:
let
  inherit (builtins) concatStringsSep;

  inherit (lib.attrsets) concatMapAttrs mapAttrsToList;
  inherit (lib.trivial) pipe;
  inherit (lib) mkIf;

  inherit (config.module.script) enable install;
  inherit (config.xdg) dataHome;
in
{
  options.module.script = {
    enable = lib.mkEnableOption "Enable script module";

    install = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      default = { };
    };
  };

  config = mkIf enable {
    home.sessionVariables.SCRIPT = "$(mktemp -d)";

    xdg.dataFile =
      concatMapAttrs
        (name: text: {
          "scripts/${name}" = {
            text =
              ''
                #!${pkgs.dash}/bin/dash
                ${text}
              '';
            executable = true;
          };
        })
        install;

    programs.bash.profileExtra =
      pipe install [
        (mapAttrsToList (str: _: "cp -f \"${dataHome}/scripts/${str}\" \"$SCRIPT/${str}\""))
        (concatStringsSep "\n")
      ];
  };
}
