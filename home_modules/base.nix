# base for all systems
{ nixpkgs, pkgs, specialArgs, ... }: with specialArgs;
{
  nixpkgs.overlays = [
    nix-alien.overlays.default
  ];

  home.packages = with pkgs; [ 
    # tooling
    htop
    traceroute
    killall
    neofetch
    mosh # probably never used

    # nix-alien
    nix-alien
    nix-index # not necessary, but recommended
    nix-index-update
  ];
}
