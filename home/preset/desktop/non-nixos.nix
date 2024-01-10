{
  LIBGL_DRIVERS_PATH = "/usr/${libp}/dri";
  LIBVA_DRIVERS_PATH = "/usr/${libp}/dri";
  __EGL_VENDOR_LIBRARY_FILENAMES = "/usr/share/glvnd/egl_vendor.d/50_mesa.json";
  LD_LIBRARY_PATH = "/usr/${libp}:/usr/${libp}/vdpau:$LD_LIBRARY_PATH";
  VK_ICD_FILENAMES = "/usr/${icd}";
}
