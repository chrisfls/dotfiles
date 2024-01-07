{ config, lib, pkgs, specialArgs, ... }:
let
  inherit (config.module.brave) enable;
  inherit (specialArgs) mesa mkIfElse;
  inherit (lib.trivial) pippe;

  pkg =
    if config.preset.non-nixos then
      pkgs.usr.wrapMesaIf true
        {
          pkg = pkgs.brave.overrideAttrs
            (old: {
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
          exe = "brave";
        }

    else
      pkgs.brave;
in
{
  options.module.brave.enable = lib.mkEnableOption "Enable brave module";

  config = lib.mkIf enable {
    home.packages = [ pkg ];

    xdg.mimeApps.defaultApplications =
      let desktop = "brave-browser.desktop";
      in {
        "application/pdf" = desktop;
        "application/x-extension-htm" = desktop;
        "application/x-extension-html" = desktop;
        "application/x-extension-shtml" = desktop;
        "application/x-extension-xht" = desktop;
        "application/x-extension-xhtml" = desktop;
        "application/xhtml+xml" = desktop;
        "text/html" = desktop;
        "x-scheme-handler/about" = desktop;
        "x-scheme-handler/chrome" = desktop;
        "x-scheme-handler/ftp" = desktop;
        "x-scheme-handler/http" = desktop;
        "x-scheme-handler/https" = desktop;
        "x-scheme-handler/unknown" = desktop;
      };

    xsession.windowManager.i3.config.modes.apps."b" =
      lib.mkIf config.module.i3wm.enable
        "exec gtk-launch brave-browser; mode default";
  };
}
