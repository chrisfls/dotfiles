{ config, lib, pkgs, ... }:
let inherit (config.modules.direnv) enable extraConfig; in {
  options.modules.direnv.enable = lib.mkEnableOption "Enable direnv module";

  config = lib.mkIf enable {
    pacman.packages = [ "extra/direnv" ];

    modules.bash.extraConfig =
      ''
        eval "$(direnv hook bash)"
      '';
    modules.fish.extraConfig =
      ''
        direnv hook fish | source
      '';
  };
}
# direnv.config.whitelist.prefix = [ "${config.xdg.userDirs.desktop}" ];
/*
  λ cat ~/.config/direnv/direnv.toml 
  [whitelist]
  prefix = ["/home/kress/Desktop"]

  ~ ─────────────────────────────────────────────────────────────────────────────────────────────────
  λ cat ~/.config/direnv/direnvrc 
  source /nix/store/widgzq59vg4i70ck8by1ns510n748899-nix-direnv-3.0
*/
