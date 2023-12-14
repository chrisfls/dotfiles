{ inputs, lib, pkgs, ... }:
let
  overlay = import inputs.nixpkgs {
    system = pkgs.system;
    overlays = [ inputs.nixgl.overlay ];
  };
in
{
  home.packages = [
    overlay.nixgl.nixGLIntel
    overlay.nixgl.nixVulkanIntel
  ];

  targets.genericLinux.enable = true;
}
