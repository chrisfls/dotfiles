{ config, lib, pkgs, ... }:
let inherit (config.pacman) enable; in lib.mkIf enable {
  home.packages = [
    (pkgs.writeShellScriptBin "nogl"
      ''
        unset __EGL_VENDOR_LIBRARY_FILENAMES
        unset LD_LIBRARY_PATH
        unset LIBGL_DRIVERS_PATH
        unset LIBVA_DRIVERS_PATH
        unset VK_ICD_FILENAMES
        exec "$@"
      '')
  ];

  home.sessionVariables = {
    LD_LIBRARY_PATH = "${config.xdg.configHome}/drivers/lib:${config.xdg.configHome}/drivers/lib/vdpau:$LD_LIBRARY_PATH";
    LIBGL_DRIVERS_PATH = "${config.xdg.configHome}/drivers/lib/dri";
    LIBVA_DRIVERS_PATH = "${config.xdg.configHome}/drivers/lib/dri";
    VK_ICD_FILENAMES = "${config.xdg.configHome}/drivers/share/vulkan/icd.d/intel_icd.x86_64.json";
    __EGL_VENDOR_LIBRARY_FILENAMES = "${config.xdg.configHome}/drivers/share/glvnd/egl_vendor.d/50_mesa.json";
  };

  xsession.importedVariables = [
    "LD_LIBRARY_PATH"
    "LIBGL_DRIVERS_PATH"
    "LIBVA_DRIVERS_PATH"
    "VK_ICD_FILENAMES"
    "__EGL_VENDOR_LIBRARY_FILENAMES"
  ];

}

#export DOCKER_HOST="unix://$XDG_RUNTIME_DIR/podman/podman.sock"
