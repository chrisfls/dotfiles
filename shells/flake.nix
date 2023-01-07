{
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let pkgs = nixpkgs.legacyPackages.${system}; in
        with pkgs;
        {
          # if this gets out of hand move them to their own files
          devShells = {
            paack = pkgs.mkShell rec {
              name = "paack";
              buildInputs = with pkgs; [
                elmPackages.elm-format
                nodejs
                yarn
                python39 # needed by pack
              ];
            };
            "fsharp-game" = pkgs.mkShell rec {
              name = "fsharp-game";
              buildInputs = with pkgs; [
                deno
                dotnet-sdk_7
              ];
            };
          };
        });
}
