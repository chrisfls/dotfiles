final: prev:
let callPackage = path: prev.callPackage path final; in {
  writeHostScriptBin = callPackage ./write-host-script-bin.nix;
}
