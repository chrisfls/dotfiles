{
  description = "kress's NixOS Flake";

  nixConfig = {
    trusted-users = [ "root" "kress" ];
    extra-substituters = "https://cache.nixos.org https://nix-community.cachix.org";
    extra-trusted-public-keys = "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs=";
    extra-experimental-features = "nix-command flakes";
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
    nix-secrets = {
      url = "git+ssh://git@github.com/kress95/nix-secrets?ref=main";
      flake = false;
    };
  };

  outputs = { self, ... }@inputs:
    with inputs;
    let
      specialArgs = {
        inherit (inputs) nixpkgs unstable home-manager agenix homeage nix-secrets;
        keys = import "${nix-secrets}/keys.nix";
      };
    in
    {
      nixosConfigurations = {
        wsl = nixpkgs.lib.nixosSystem {
          inherit specialArgs;
          system = "x86_64-linux";
          modules = [
            nixos-wsl.nixosModules.wsl
            home-manager.nixosModules.home-manager
            ./per-system/wsl
          ];
        };
      };
      homeConfigurations = {
        kress = home-manager.lib.homeManagerConfiguration {
          pkgs = import nixpkgs {
            system = "x86_64-linux";
            config.allowUnfree = true;
          };
          modules = [
            homeage.homeManagerModules.homeage
            ./per-system/arch-wsl-rmxp/per-user/kress
          ];
          extraSpecialArgs = specialArgs // {
            distro = "Arch";
            nixosConfig = {
              networking = { hostName = "arch-wsl-rmxp"; };
            };
          };
        };
      };
    };
}
