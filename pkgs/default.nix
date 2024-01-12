final: prev:
let
  callPackage = path: prev.callPackage path prev;
in
{
  extra = {
    attrsets = import ./attrsets.nix;
    mk-if-else = import ./mk-if-else.nix;
    qt = import ./qt.nix prev;
    string = import ./string.nix;
  };
}
