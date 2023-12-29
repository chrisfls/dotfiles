{ config, lib, pkgs, ... }:
let
  inherit (builtins)
    attrNames
    attrValues
    concatStringsSep
    filter
    map;

  inherit (lib.attrsets)
    mapAttrs';

  inherit (lib.strings)
    splitString
    removePrefix
    removeSuffix;

  inherit (lib.trivial)
    pipe;

  inherit (lib)
    types
    isAttrs;

  cfg = config.module.sxhkd;
in
{
  options.module.sxhkd = {
    enable = lib.mkEnableOption "Enable sxhkd module";
    keybindings = lib.mkOption {
      type = types.lazyAttrsOf (types.oneOf [ types.str (types.lazyAttrsOf types.str) ]);
      default = { "super + Escape" = "pkill -USR1 -x sxhkd"; };
    };
  };

  config = lib.mkIf cfg.enable {
    # reload sxhkd configs
    module.sxhkd.keybindings."super + Escape" = "pkill -USR1 -x sxhkd";

    xsession.initExtra =
      ''
        export SXHKD_FIFO="$XDG_RUNTIME_DIR/sxhkd.fifo"
        [ -e "$SXHKD_FIFO" ] && rm "$SXHKD_FIFO"
        mkfifo "$SXHKD_FIFO"
        systemctl --user import-environment SXHKD_FIFO
      '';

    home.sessionVariables.SXHKD_SHELL = "${pkgs.dash}/bin/dash";

    services.sxhkd = {
      enable = true;
      package = pkgs.sxhkd.overrideAttrs (old: {
        src = pkgs.fetchFromGitHub {
          owner = "baskerville";
          repo = "sxhkd";
          rev = "b0923b6b3d5d85b1ff80a5bb286934b8721a7c08";
          hash = "sha256-cSJZveU4quKT2MG1H8/cz9JEaj6Z4t0AEViPah4QoPs=";
        };
        patches = old.patches ++ [
          (pkgs.fetchpatch {
            name = "sxhkd-line-length.patch";
            url = "https://github.com/kress95/sxhkd/commit/d8ee372042618d6b603fce19d8e3035095f1fe28.patch";
            hash = "sha256-DzFtui42M9fUB7wXkgJzGmZtC6pgQXaZhKoOJ7cTB0A=";
          })
        ];
      });
      extraOptions = [ "-s $SXHKD_FIFO" ];
      extraConfig =
        ''
          super+a;any+{a-z}
            :
        '';
      keybindings =
        let
          trim = str:
            let
              str' = (removePrefix " " (removeSuffix " " str));
            in
            if str == str' then
              str
            else
              trim str';

          format = str:
            pipe str [
              (splitString "\n")
              (map trim)
              (filter (str: str != ""))
              (concatStringsSep " ")
            ];
        in
        mapAttrs'
          (name: value:
            if isAttrs value then
              let
                names = attrNames value;
                values = attrValues value;
              in
              {
                name = "${name} {${concatStringsSep ", " names}}";
                value = "{ ${concatStringsSep " , " (map format values)} }";
              }

            else
              { inherit name; value = format value; })
          cfg.keybindings;
    };
  };
}
