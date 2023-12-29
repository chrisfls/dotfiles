{ config, lib, pkgs, ... }:
with {
  inherit (builtins) replaceStrings readFile;
};
let
  cfg = config.module.browser;

  browser = "brave-browser.desktop";

  pkg = pkgs.brave;

  # the wrapper is injecting mesa/libva...
  wrapper = pkgs.symlinkJoin {
    name = "brave";
    paths = [
      (pkgs.writeShellScriptBin "brave"
        (replaceStrings
          [ "mesa" "libva" ]
          [ "dummy-mesa" "dummy-libva" ]
          (readFile "${pkg}/bin/brave")))
      pkg
    ];
  };
in
{
  options.module.browser.enable = lib.mkEnableOption "Enable browser module";

  config = lib.mkIf cfg.enable {
    home.packages = [ wrapper ];

    xdg.mimeApps = {
      enable = true;
      defaultApplications = {
        "x-scheme-handler/http" = browser;
        "x-scheme-handler/https" = browser;
        "x-scheme-handler/chrome" = browser;
        "text/html" = browser;
        "application/x-extension-htm" = browser;
        "application/x-extension-html" = browser;
        "application/x-extension-shtml" = browser;
        "application/xhtml+xml" = browser;
        "application/x-extension-xhtml" = browser;
        "application/x-extension-xht" = browser;
      };
    };

    module.sxhkd.keybindings."super + a; b" = "gtk-launch brave-browser";
  };
}
