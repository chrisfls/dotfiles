{ pkgs, ... }:
{
  targets.genericLinux.enable = true;

  home.sessionVariables = {
    LIBGL_DRIVERS_PATH = "/usr/lib/dri";
    LIBVA_DRIVERS_PATH = "/usr/lib/dri";
    __EGL_VENDOR_LIBRARY_FILENAMES = "/usr/share/glvnd/egl_vendor.d/50_mesa.json";
    # vulkan  will not work without 
    # zlib-*/lib
    # libdrm-*/lib
    # libX11-*/lib
    # libxcb-*/lib
    # libxshmfence-*/lib
    # wayland-*/lib
    # gcc-*/lib
    LD_LIBRARY_PATH = "/usr/lib:/usr/lib/vdpau:$LD_LIBRARY_PATH";
    VK_ICD_FILENAMES = "$(find /usr/share/vulkan/icd.d/ -name '*.json' | tr '\\n' ':' | sed 's/:$//')";
  };
}
