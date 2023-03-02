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
    nix-secrets = {
      url = "git+ssh://git@github.com/kress95/nix-secrets?ref=main";
      flake = false;
    };
  };

  outputs = { self, ... }@inputs:
    with inputs;
    let
      specialArgs = {
        inherit (inputs) nixpkgs home-manager agenix homeage nix-secrets;
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
            ./per-system/arch-wsl-rpgmxp/per-user/kress
          ];
          extraSpecialArgs = specialArgs // {
            distro = "Arch";
            nixosConfig = {
              networking = { hostName = "arch-wsl-rpgmxp"; };
            };
          };
        };
      };
    };
}
