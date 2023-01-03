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
    vscode-server = {
      url = "github:msteen/nixos-vscode-server";
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
    cloudflareCertAuthFile = {
      type = "file";
      url = "https://developers.cloudflare.com/cloudflare-one/static/documentation/connections/Cloudflare_CA.crt";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, ... }@inputs:
    let
      userPathFor = username: hostname: "${username}@${hostname}";
      specialArgs = {
        inherit (inputs) nixpkgs agenix homeage cloudflareCertAuthFile;
        # helpers
        home = path: ./assets/home/${path};
        misc = path: ./assets/misc/${path};
        keys = import ./secrets/keys.nix;
        secretPath = username: hostname: path:
          ./secrets/users/${userPathFor username hostname}/${path};
        importUser = username: hostname:
          import ./users/${userPathFor username hostname}/home.nix
            { inherit username hostname; };
        compose = with nixpkgs; lib.trivial.flip lib.trivial.pipe; # eff da police (see: bit.ly/3IiZTw9)
        # global config
        defaultUser = "kress";
      };
    in
    {
      nixosConfigurations = {
        wsl =
          let
            system = "x86_64-linux";
            modules = [
              inputs.nixos-wsl.nixosModules.wsl
              inputs.vscode-server.nixosModule
              inputs.home-manager.nixosModules.home-manager
              ./systems/configuration.nix
              ./systems/wsl/configuration.nix
              ./systems/wsl/users.nix
            ];
          in
          nixpkgs.lib.nixosSystem { inherit system modules specialArgs; };
      };
    };
}
