# commong fish settings
{ pkgs, ... }:
{
  home.packages = with pkgs; [ 
    fish
    fishPlugins.autopair-fish # probably not needed
    fishPlugins.colored-man-pages
    fishPlugins.done # probably never used
    fishPlugins.foreign-env # probably not needed
    fishPlugins.sponge
    fishPlugins.tide
  ];

  programs.fish = {
    enable = true;
    shellAliases = {
      "e" = "code";
      "da" = "direnv allow";
      "g" = "git";
      "ns" = "nix search nixpkgs";
      # system management
      "s" = "git --git-dir=$HOME/.system.git --work-tree=/etc/nixos";
    };
  };
}
