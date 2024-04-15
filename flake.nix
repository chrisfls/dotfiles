{
  description = "chris's NixOS Flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    agenix.url = "github:ryantm/agenix";
    agenix.inputs.home-manager.follows = "home-manager";
    agenix.inputs.nixpkgs.follows = "nixpkgs";
    agenix.inputs.systems.follows = "systems";

    homeage.url = "github:jordanisaacs/homeage";
    homeage.inputs.nixpkgs.follows = "nixpkgs";

    systems.url = "github:nix-systems/default";
  };

  nixConfig = {
    extra-trusted-public-keys = "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw= nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs=";
    extra-substituters = "https://devenv.cachix.org https://nix-community.cachix.org";
  };

  outputs = { nixpkgs, home-manager, ... }@inputs:
    let
      specialArgs = import ./special inputs;
      inherit (specialArgs) ssot;
      forEachSystem = nixpkgs.lib.genAttrs (import inputs.systems);
    in
    {
      homeConfigurations = {
        "${ssot.users.arch-rmxp.chris.id}" = home-manager.lib.homeManagerConfiguration rec {
          pkgs = import inputs.nixpkgs {
            system = "x86_64-linux";
            overlays = [ (import ./pkgs) ];
          };

          extraSpecialArgs = specialArgs;

          modules = [
            ./home/modules
            ./home/presets
            ./home/users/${ssot.users.arch-rmxp.chris.id}.nix
          ];
        };
      };
    };
}
