{
  description = "kress's NixOS Flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    agenix.url = "github:ryantm/agenix";

    homeage.url = "github:jordanisaacs/homeage";
  };

  outputs = { nixpkgs, home-manager, homeage, ... }@inputs:
    with inputs;
    let
      ssot = import ./ssot.nix;
      specialArgs = {
        inherit ssot homeage;
        flakes = inputs;
      };
    in
    {
      homeConfigurations = {
        "${ssot.users.arch-rmxp.kress.id}" = home-manager.lib.homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages.x86_64-linux;
          modules = [
            ./home/all
            ./home/desk
            ./home/dev
            ./home/work/paack.nix
            ./home/${ssot.users.arch-rmxp.kress.id}.nix
          ];
          extraSpecialArgs = specialArgs;
        };
      };
    };
}
