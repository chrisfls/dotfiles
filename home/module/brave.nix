{ config, lib, pkgs, ... }:
with {
  inherit (builtins) replaceStrings readFile;
};
let
  cfg = config.module.brave;

  brave = "brave-browser.desktop";

  pkg = pkgs.brave;

  # the wrapper is injecting mesa/libva...
  wrapper = pkgs.symlinkJoin {
    name = "brave";
    paths = [
      (pkgs.writeShellScriptBin "brave"
        (replaceStrings
          [ "mesa" "libva" "--enable-features=" ]
          [
            "dummy-mesa"
            "dummy-libva"
            "--enable-features=VaapiIgnoreDriverChecks,VaapiVideoDecodeLinuxGL,"
          ]
          (readFile "${pkg}/bin/brave")))
      pkg
    ];
  };
in
{
  options.module.brave.enable = lib.mkEnableOption "Enable brave module";

  config = lib.mkIf cfg.enable {
    home.packages = [ wrapper ];

    xdg.mimeApps = {
      enable = true;
      defaultApplications = {
        "x-scheme-handler/http" = brave;
        "x-scheme-handler/https" = brave;
        "x-scheme-handler/chrome" = brave;
        "text/html" = brave;
        "application/x-extension-htm" = brave;
        "application/x-extension-html" = brave;
        "application/x-extension-shtml" = brave;
        "application/xhtml+xml" = brave;
        "application/x-extension-xhtml" = brave;
        "application/x-extension-xht" = brave;
      };
    };

    module.sxhkd.keybindings."super + a; b" = "gtk-launch brave-browser";
  };
}
