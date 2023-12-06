{
  description = "kress's NixOS Flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    systems.url = "github:nix-systems/default";
    devenv.url = "github:cachix/devenv";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    homeage = {
      url = "github:jordanisaacs/homeage";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  nixConfig = {
    extra-trusted-public-keys = "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw=";
    extra-substituters = "https://devenv.cachix.org";
  };

  outputs = { nixpkgs, devenv, home-manager, homeage, systems, ... }@inputs:
    with inputs;
    let
      ssot = import ./ssot.nix;
      specialArgs = {
        inherit ssot homeage;
        flakes = inputs;
      };
      forEachSystem = nixpkgs.lib.genAttrs (import systems);
    in
    {
      packages = forEachSystem (system: {
        devenv = devenv.packages.${system}.devenv;
      });

      homeConfigurations = {
        "${ssot.users.arch-rmxp.kress.id}" = home-manager.lib.homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages.x86_64-linux;
          modules = [
            ./home/all
            ./home/desk
            ./home/dev
            ./home/work/paack
            ./home/${ssot.users.arch-rmxp.kress.id}.nix
          ];
          extraSpecialArgs = specialArgs;
        };
      };
    };
}
