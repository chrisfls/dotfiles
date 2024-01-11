{ config, lib, pkgs, ... }:
let
  enable = config.presets.non-nixos;
in
{
  options.presets.non-nixos = lib.mkEnableOption "Enable non-nixos preset";

  config = (lib.mkIf enable {
    targets.genericLinux.enable = true;
    xsession.importedVariables = [ "PATH" ];
    programs.fish.shellInit =
      ''
        set -e LIBGL_DRIVERS_PATH
        set -e LIBVA_DRIVERS_PATH
        set -e __EGL_VENDOR_LIBRARY_FILENAMES
        set -e LD_LIBRARY_PATH
        set -e VK_ICD_FILENAMES
      '';
  });
}
