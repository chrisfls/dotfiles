final: prev:
let
  callPackage = path: prev.callPackage path final;
in
{
  extra = {
    attrsets = import ./attrsets.nix;
    lpm = callPackage ./lpm.nix;
    mkIfElse = callPackage import ./mk-if-else.nix;
    qt = import ./qt.nix prev;
    string = import ./string.nix;
  };

  mopidy-youtube = callPackage ./mopidy-youtube.nix;
}
