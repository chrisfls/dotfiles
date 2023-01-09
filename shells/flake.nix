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
            equalizer = pkgs.mkShell rec {
              name = "equalizer";
              buildInputs = with pkgs; [
                deno
                libsndfile
                # TODO: use pip2nix to generate this list
                (python3.withPackages (p: with p; [
                  pillow
                  matplotlib
                  pandas
                  scipy
                  numpy
                  tabulate
                  soundfile
                  pyyaml
                  tqdm
                ]))
              ];
            };
          };
        });
}
