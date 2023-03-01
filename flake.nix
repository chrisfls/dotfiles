{
  description = "kress's NixOS Flake";

  nixConfig = {
    extra-experimental-features = "nix-command flakes";
    extra-substituters = "https://nrdxp.cachix.org https://nix-community.cachix.org";
    extra-trusted-public-keys = "nrdxp.cachix.org-1:Fc5PSqY2Jm1TrWfm88l6cvGWwz3s93c6IOifQWnhNW4= nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs=";
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nixos-wsl = {
      url = "github:nix-community/NixOS-WSL";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager/master";
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
    nix-alien = {
      url = "github:thiagokokada/nix-alien";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-secrets = {
      url = "github:kress95/nix-secrets";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, home-manager, nix-secrets, ... }@inputs:
    let
      specialArgs = {
        inherit (inputs) nixpkgs nix-secrets home-manager agenix homeage nix-alien nixos-wsl;
        keys = import "${nix-secrets}/keys.nix";
      };
    in
    {
      nixosConfigurations = {
        wsl = nixpkgs.lib.nixosSystem {
          inherit specialArgs;
          system = "x86_64-linux";
          modules = [ ./systems/wsl.nix ];
        };
      };
      homeConfigurations = {
        kress =
          let
            system = "x86_64-linux";
            pkgs = import nixpkgs {
              inherit system;
              config.allowUnfree = true;
            };
            modules = [
              ./home_modules/not_nixos.nix
              ./home_modules/base_devel.nix
              ./home_modules/fish.nix
              ./home_modules/fish_wsl.nix
              ./home_modules/keychain.nix
              ./home_modules/git.nix
              ./home_modules/micro.nix
              ./home_modules/direnv.nix
              ./systems/arch/kress.nix
            ];
          in
          home-manager.lib.homeManagerConfiguration {
            inherit pkgs modules;
            extraSpecialArgs = specialArgs;
          };
      };
    };
}
