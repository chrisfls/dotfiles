{ config, lib, pkgs, specialArgs, ... }:
let
  # TODO: pacman

  inherit (config.modules.microsoft-edge) enable;

  pkg =
    if config.presets.non-nixos then
      pkgs.microsoft-edge.overrideAttrs
        (old: {
          postFixup =
            ''
              substituteInPlace $out/bin/microsoft-edge \
                --replace mesa dummy-mesa
              substituteInPlace $out/bin/microsoft-edge \
                --replace libva dummy-libva
              substituteInPlace $out/bin/microsoft-edge \
                --replace "--enable-features=" "--force-device-scale-factor=1.5 --enable-features=VaapiVideoDecodeLinuxGL,"
            '';
        })

    else
      pkgs.microsoft-edge;
in
{
  options.modules.microsoft-edge.enable = lib.mkEnableOption "Enable microsoft-edge module";

  config = lib.mkIf enable {
    home.packages = [ pkg ];
    modules.i3wm.apps."shift+b" = "microsoft-edge";
  };
}
