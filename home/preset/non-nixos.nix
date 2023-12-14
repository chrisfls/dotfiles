{ pkgs, ... }:
{
  home.packages = [
    pkgs.nixgl.nixGLIntel
    pkgs.nixgl.nixVulkanIntel
  ];

  targets.genericLinux.enable = true;
}
