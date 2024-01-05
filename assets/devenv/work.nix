{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
    systems.url = "github:nix-systems/default";
    devenv.url = "github:cachix/devenv";
  };

  nixConfig = {
    extra-trusted-public-keys = "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw=";
    extra-substituters = "https://devenv.cachix.org";
  };

  outputs = { self, nixpkgs, devenv, systems, ... } @ inputs:
    let
      forEachSystem = nixpkgs.lib.genAttrs (import systems);
    in
    {
      packages = forEachSystem (system: {
        devenv-up = self.devShells.${system}.default.config.procfileScript;
      });

      devShells = forEachSystem
        (system:
          let
            pkgs = import nixpkgs {
              inherit system;
              config.permittedInsecurePackages = [
                # required by elm lsp
                "nodejs-16.20.2"

                # needed for postgres
                "openssl-1.1.1w"
              ];
            };

            otp = pkgs.beam.packages.erlangR23;
          in
          {
            default = devenv.lib.mkShell {
              inherit inputs pkgs;
              modules = [
                {
                  packages = [
                    pkgs.elmPackages.elm-format
                    pkgs.elmPackages.elm-language-server
                    pkgs.nodejs-16_x # vscode's nodejs version for elm lsp
                    pkgs.python39 # needed for node gyp
                    pkgs.asdf-vm # probably will not even gonna use it
                    pkgs.erlang_23
                    pkgs.elixir_1_14
                    pkgs.inotify-tools
                    pkgs.rebar3
                  ];

                  services.postgres = {
                    enable = true;
                    listen_addresses = "localhost";
                    initialScript =
                      ''
                        CREATE ROLE postgres WITH SUPERUSER LOGIN PASSWORD 'postgres';
                      '';
                    initialDatabases = [{ name = "core_dev"; }];
                  };

                  enterShell = ''
                    mkdir -p .nix-mix
                    mkdir -p .nix-hex
                    export MIX_HOME=$PWD/.nix-mix
                    export HEX_HOME=$PWD/.nix-hex
                    export PATH=$MIX_HOME/bin:$PATH
                    export PATH=$HEX_HOME/bin:$PATH
                    export LANG=en_US.UTF-8
                    export ERL_AFLAGS="-kernel shell_history enabled"
                    export ERL_LIBS=$HEX_HOME/lib/erlang/lib
                  '';
                }
              ];
            };
          });
    };
}
