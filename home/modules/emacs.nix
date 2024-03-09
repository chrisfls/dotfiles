{ config, lib, pkgs, ... }:
let
  inherit (config.modules.emacs) enable;

  rev = "03d692f129633e3bf0bd100d91b3ebf3f77db6d1";
  configHome = config.xdg.configHome;
  dir = "${configHome}/emacs";

  doom-install =
    pkgs.writeHostScriptBin "doom-install"
      ''
        mkdir -p ${dir}
        git init ${dir}
        git -C ${dir} remote add origin git@github.com:doomemacs/doomemacs.git
        git -C ${dir} fetch origin ${rev}
        git -C ${dir} reset --hard FETCH_HEAD
        ${dir}/bin/doom build
      '';
in
{
  options.modules.emacs.enable = lib.mkEnableOption "Enable emacs module";

  config = lib.mkIf enable {
    pacman.packages = [
      "extra/emacs"
      "aur/semgrep-bin"
      "extra/ripgrep"
    ];

    home.packages = [ doom-install ];

    home.sessionPath = [ "${dir}/bin" ];

    modules.copyFile."${configHome}/doom" = "${toString ../../assets/doom}";
  };
}
