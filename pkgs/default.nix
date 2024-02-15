final: prev:
let
  callPackage = path: prev.callPackage path final;
in
{
  extra = {
    # lpm = callPackage ./lpm.nix;
    attrsets = import ./attrsets.nix;
    mkIfElse = import ./mk-if-else.nix final;
    qt = import ./qt.nix final;
    string = import ./string.nix;
  };

  mopidy-youtube = callPackage ./mopidy-youtube.nix;
}
