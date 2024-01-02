{ config, lib, pkgs, ... }:
with {
  inherit (builtins) replaceStrings readFile;
};
let
  cfg = config.module.brave;

  brave = "brave-browser.desktop";

  pkg = pkgs.brave.overrideAttrs (old: {
    postFixup =
      ''
        substituteInPlace $out/bin/brave \
          --replace mesa dummy-mesa
        substituteInPlace $out/bin/brave \
          --replace libva dummy-libva
        substituteInPlace $out/bin/brave \
          --replace "--enable-features=" "--enable-features=VaapiVideoDecodeLinuxGL,"
      '';
  });
in
{
  options.module.brave.enable = lib.mkEnableOption "Enable brave module";

  config = lib.mkIf cfg.enable {
    home.packages = [ pkg ];

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

    # module.sxhkd.keybindings."super + a; b" = "gtk-launch brave-browser";

    xsession.windowManager.i3.config.modes.apps."b" = "exec gtk-launch brave-browser";
  };
}
