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
    nix-ld = {
      url = "github:Mic92/nix-ld";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, ... }@inputs:
    let
      userPath = username: hostname: "users/${username}@${hostname}";

      # helpers

      fileFromHome = path: ./assets/home/${path};

      fileFromMisc = path: ./assets/misc/${path};

      fileFromSecrets = { username, hostname, ... }: path:
        ./secrets/${userPath username hostname}/${path};

      keys = import ./secrets/keys.nix;

      importUser = username: hostname:
        import ./${userPath username hostname}/home.nix { inherit username hostname; };

      homeageConfigUser = { username, hostname, ... }@userArgs: { identities, file }: {
        identityPaths = map (path: "/home/${username}/${path}") identities;
        installationType = "activation";
        file = nixpkgs.lib.attrsets.mapAttrs'
          (target: source:
            {
              name = toString source;
              value = {
                source = fileFromSecrets userArgs source;
                copies = [ "/home/${username}/${target}" ];
              };
            }
          )
          file;
      };

      # eff da police (see: bit.ly/3IiZTw9)
      # compose = nixpkgs.lib.trivial.flip nixpkgs.lib.trivial.pipe; 

      specialArgs = {
        inherit (inputs) nixpkgs agenix homeage;
        inherit fileFromHome fileFromMisc fileFromSecrets;
        inherit keys importUser homeageConfigUser;
        # global config
        defaultUser = "kress";
      };
    in
    {
      nixosConfigurations = {
        wsl =
          let
            system = "x86_64-linux";
            modules = with inputs; [
              nixos-wsl.nixosModules.wsl
              nix-ld.nixosModules.nix-ld
              vscode-server.nixosModule
              home-manager.nixosModules.home-manager
              ./systems/configuration.nix
              ./systems/wsl/configuration.nix
              ./systems/wsl/users.nix
            ];
          in
          nixpkgs.lib.nixosSystem { inherit system modules specialArgs; };
      };
    };
}
