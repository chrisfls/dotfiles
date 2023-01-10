{ pkgs ? import <nixpkgs> { } }:
with pkgs;
mkShell {
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
}
