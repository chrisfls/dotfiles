{
  description = "kress's NixOS Flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    systems.url = "github:nix-systems/default";

    flake-utils.url = "github:numtide/flake-utils";
    flake-utils.inputs.systems.follows = "systems";

    flake-compat.url = "github:edolstra/flake-compat";

    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    pre-commit-hooks.inputs.nixpkgs.follows = "nixpkgs";
    pre-commit-hooks.inputs.flake-utils.follows = "flake-utils";
    pre-commit-hooks.inputs.flake-compat.follows = "flake-compat";

    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    agenix.url = "github:ryantm/agenix";
    agenix.inputs.home-manager.follows = "home-manager";
    agenix.inputs.nixpkgs.follows = "nixpkgs";
    agenix.inputs.systems.follows = "systems";

    homeage.url = "github:jordanisaacs/homeage";
    homeage.inputs.nixpkgs.follows = "nixpkgs";
  };

  nixConfig = {
    extra-trusted-public-keys = "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs=";
    extra-substituters = "https://nix-community.cachix.org";
  };

  outputs = { nixpkgs, home-manager, ... }@inputs:
    let
      inherit (import ./special inputs) specialArgs mkExtraSpecialArgs;
      inherit (specialArgs) ssot;
      forEachSystem = nixpkgs.lib.genAttrs (import inputs.systems);
    in
    {
      homeConfigurations = {
        "${ssot.users.arch-rmxp.kress.id}" = home-manager.lib.homeManagerConfiguration rec {
          pkgs = import inputs.nixpkgs {
            system = "x86_64-linux";
            overlays = [ (import ./pkgs) ];
          };

          extraSpecialArgs = mkExtraSpecialArgs pkgs;

          modules = [
            ./home/modules
            ./home/preset
            ./home/user/${ssot.users.arch-rmxp.kress.id}.nix
          ];
        };
      };
    };
}
