# put dash scripts in the tmp and call them with $SCRIPT/name
{ config, lib, pkgs, ... }:
let inherit (config.modules.script) enable install; in {
  options.modules.script = {
    enable = lib.mkEnableOption "Enable script module";

    install = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      default = { };
    };
  };

  config = lib.mkIf enable {
    home.sessionVariables.SCRIPT = "$(mktemp -d)";

    xsession.importedVariables = [ "SCRIPT" ];

    xdg.dataFile =
      lib.attrsets.concatMapAttrs
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
      lib.trivial.pipe install [
        (lib.attrsets.mapAttrsToList
          (str: _: "cp -f \"${config.xdg.dataHome}/scripts/${str}\" \"$SCRIPT/${str}\""))
        (builtins.concatStringsSep "\n")
      ];
  };
}