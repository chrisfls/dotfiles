{ flakes, pkgs, ... }:
let
  overlay = import flakes.nixpkgs {
    system = pkgs.system;
    overlays = [ flakes.nixgl.overlay ];
  };
in
{
  home.packages = [
    overlay.nixgl.nixGLIntel
    overlay.nixgl.nixVulkanIntel
  ];

  programs.bash.sessionVariables.XDG_DATA_DIRS = "$HOME/.nix-profile/share:$XDG_DATA_DIRS";


  xdg.configFile = {
    "hypr/hyprland.conf".source = ./hyprland.conf;
  };

  # programs.bash.profileExtra = ''
  #   source <(
  #     nixGLIntel /bin/bash -c "\
  #       echo \"export __EGL_VENDOR_LIBRARY_FILENAMES=\\\"\$__EGL_VENDOR_LIBRARY_FILENAMES:\$__EGL_VENDOR_LIBRARY_FILENAMES\\\"\"; \
  #       echo \"export LD_LIBRARY_PATH=\\\"\$LD_LIBRARY_PATH:\$LD_LIBRARY_PATH\\\"\"; \
  #       echo \"export LIBGL_DRIVERS_PATH=\\\"\$LIBGL_DRIVERS_PATH:\$LIBGL_DRIVERS_PATH\\\"\"; \
  #       echo \"export LIBVA_DRIVERS_PATH=\\\"\$LIBVA_DRIVERS_PATH:\$LIBVA_DRIVERS_PATH\\\"\";"
  #   )
  #   source <(
  #     libVulkanIntel /bin/bash -c "\
  #       echo \"export LD_LIBRARY_PATH=\\\"\$LD_LIBRARY_PATH:\$LD_LIBRARY_PATH\\\"\"; \
  #       echo \"export VK_ICD_FILENAMES=\\\"\$VK_ICD_FILENAMES:\$VK_ICD_FILENAMES\\\"\"; \
  #       echo \"export VK_LAYER_PATH=\\\"\$VK_LAYER_PATH:\$VK_LAYER_PATH\\\"\";"
  #   )
  # '';
}
