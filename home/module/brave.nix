{ config, lib, pkgs, ... }:
let
  inherit (config.module.brave) enable;

  brave = "brave-browser.desktop";

  # pkg = pkgs.brave.overrideAttrs (old: {
  #   postFixup =
  #     ''
  #       substituteInPlace $out/bin/brave \
  #         --replace mesa dummy-mesa
  #       substituteInPlace $out/bin/brave \
  #         --replace libva dummy-libva
  #       substituteInPlace $out/bin/brave \
  #         --replace "--enable-features=" "--enable-features=VaapiVideoDecodeLinuxGL,"
  #     '';
  # });
in
{
  options.module.brave.enable = lib.mkEnableOption "Enable brave module";

  config = lib.mkIf enable {
    home.packages = [ pkgs.usr.brave ];
    pacman.overrides.brave = [ "chaotic-aur/brave-bin" ];

    xdg.mimeApps.defaultApplications = {
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

    # module.sxhkd.keybindings."super + a; b" = "gtk-launch brave-browser";

    xsession.windowManager.i3.config.modes.apps."b" = lib.mkIf config.module.i3wm.enable "exec gtk-launch brave-browser";
  };
}
