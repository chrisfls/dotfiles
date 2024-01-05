inputs:
rec {
  specialArgs = {
    inherit inputs;
    attrsets = import ./attrsets.nix;
    color-schemes = import ./color-schemes.nix;
    string = import ./string.nix;
    ssot = import ./ssot.nix;
  };

  mkExtraSpecialArgs = args: specialArgs // {
    qt = import ./qt.nix args;
    mkIfElse = import ./mk-if-else.nix args;
    mesa = import ./mesa.nix args;
  };
}
