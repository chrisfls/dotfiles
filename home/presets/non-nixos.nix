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

  programs.bash.sessionVariables.XDG_DATA_DIRS = "$HOME/.nix-profile/share:$XDG_DATA_DIRS";

  targets.genericLinux.enable = true;
}
