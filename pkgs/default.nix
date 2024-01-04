final: prev:
let
  callPackage = path: prev.callPackage path prev;
in
{
  #usr = callPackage (import ./usr.nix);
  #sublime4 = callPackage (import ./sublime4.nix);
  #sublime4-dev = callPackage (import ./sublime4-dev.nix);
  #sublime-merge-dev = callPackage (import ./sublime-merge-dev.nix);
  #sublime-merge = callPackage (import ./sublime-merge.nix);
}
