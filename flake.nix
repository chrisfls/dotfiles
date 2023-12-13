{
  description = "kress's NixOS Flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";

    systems.url = "github:nix-systems/default";

    flake-utils.url = "github:numtide/flake-utils";
    flake-utils.inputs.systems.follows = "systems";

    flake-compat.url = "github:edolstra/flake-compat";

    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    pre-commit-hooks.inputs.nixpkgs.follows = "nixpkgs";
    pre-commit-hooks.inputs.flake-utils.follows = "flake-utils";
    pre-commit-hooks.inputs.flake-compat.follows = "flake-compat";

    devenv.url = "github:cachix/devenv";
    devenv.inputs.nixpkgs.follows = "nixpkgs";
    devenv.inputs.pre-commit-hooks.follows = "pre-commit-hooks";
    devenv.inputs.flake-compat.follows = "flake-compat";

    nixgl.url = "github:guibou/nixGL";
    nixgl.inputs.nixpkgs.follows = "nixpkgs";
    nixgl.inputs.flake-utils.follows = "flake-utils";

    home-manager.url = "github:nix-community/home-manager/release-23.11";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    agenix.url = "github:ryantm/agenix";
    agenix.inputs.nixpkgs.follows = "nixpkgs";
    agenix.inputs.home-manager.follows = "home-manager";

    homeage.url = "github:jordanisaacs/homeage";
    homeage.inputs.nixpkgs.follows = "nixpkgs";
  };

  nixConfig = {
    extra-trusted-public-keys = "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs= devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw=";
    extra-substituters = "https://nix-community.cachix.org https://devenv.cachix.org";
  };

  outputs = { nixpkgs, devenv, home-manager, systems, ... }@inputs:
    let
      attrsets = import ./extra/attrsets.nix nixpkgs;
      hm = import ./extra/hm.nix nixpkgs;
      ssot = import ./extra/ssot.nix;
      string = import ./extra/string.nix;
      homeSpecialArgs = {
        inherit inputs;
        extra = { inherit attrsets hm ssot string; };
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
          modules = [ ./user/${ssot.users.arch-rmxp.kress.id}.nix ];
          extraSpecialArgs = homeSpecialArgs;
        };
      };
    };
}
