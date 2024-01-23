{ config, lib, pkgs, ... }:
let
  inherit (config.presets) non-nixos;
  inherit (config.modules.emacs) enable;

  rev = "03d692f129633e3bf0bd100d91b3ebf3f77db6d1";
  configHome = config.xdg.configHome;
  dir = "${configHome}/emacs";
in
{
  options.modules.emacs.enable = lib.mkEnableOption "Enable emacs module";

  config = lib.mkIf enable {
    home.packages =
      if non-nixos then [ ]
      else [ pkgs.emacs29 pkgs.semgrep ];

    pacman.packages = [ "extra/emacs-nativecomp" ];

    home.activation.doomemacs = lib.hm.dag.entryAfter [ "writeBoundary" ]
      ''
        if [ ! -d ${dir} ]; then
          $DRY_RUN_CMD mkdir -p ${dir}
          $DRY_RUN_CMD export PATH="${pkgs.emacs29}/bin:$PATH"
          $DRY_RUN_CMD git init ${dir}
          $DRY_RUN_CMD git -C ${dir} remote add origin git@github.com:doomemacs/doomemacs.git
          $DRY_RUN_CMD git -C ${dir} fetch origin ${rev}
          $DRY_RUN_CMD git -C ${dir} reset --hard FETCH_HEAD
          $DRY_RUN_CMD ${dir}/bin/doom install
          $DRY_RUN_CMD rm ${configHome}/doom/config.el ${configHome}/doom/init.el ${configHome}/doom/packages.el
        fi
      '';

    home.sessionPath = [ "${dir}/bin" ];

    modules.copyFile = {
      "${configHome}/doom" = "${toString ../../assets/doom}";
    };
  };
}
