nixpkgs:
{
  attrsets = import ./attrsets.nix;
  color-schemes = import ./color-schemes.nix;
  ssot = import ./ssot.nix;
  string = import ./string.nix;
  qt = import ./qt.nix nixpkgs;
}
