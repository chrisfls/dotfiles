final: prev:
let callPackage = path: prev.callPackage path final; in {
  extra = {
    attrsets = import ./attrsets.nix;
    mkIfElse = import ./mk-if-else.nix final;
    qt = import ./qt.nix final;
    string = import ./string.nix;
  };
}
