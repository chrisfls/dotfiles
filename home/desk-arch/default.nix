{ flakes, pkgs, ... }:
let
  overlay = import flakes.nixpkgs {
    system = pkgs.system;
    overlays = [ flakes.nixgl.overlay ];
  };

  wrapGL = name: pkg:
    pkgs.writeShellApplication {
      inherit name;
      text = "${pkgs.lib.meta.getExe overlay.nixgl.nixGLIntel} ${pkgs.lib.meta.getExe pkg}";
    };

  wrapVulkan = name: pkg:
    pkgs.writeShellApplication {
      inherit name;
      text = "${pkgs.lib.meta.getExe overlay.nixgl.nixVulkanIntel} ${pkgs.lib.meta.getExe pkg}";
    };
in
{
  home.packages = [
    overlay.nixgl.nixGLIntel
    overlay.nixgl.nixVulkanIntel
  ];

  programs.waybar.package = wrapGL "waybar" pkgs.waybar;
  # wayland.windowManager.hyprland.package = wrapGL "hyprland" pkgs.hyprland;
  programs.kitty.package = wrapGL "kitty" pkgs.kitty;

  programs.bash.sessionVariables.XDG_DATA_DIRS = "$HOME/.nix-profile/share:$XDG_DATA_DIRS";

  xdg.configFile = {
    #"hypr/hyprland.conf".source = ./hyprland.conf;
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
