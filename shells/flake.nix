{
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          attrs = { inherit pkgs; };
        in
        with pkgs;
        {
          devShells.paack = import ./paack/shell.nix attrs;
        });
}
