{ stdenvNoCC, ... }:
stdenvNoCC.mkDerivation {
  pname = "usr";
  version = "latest";
  dontUnpack = true;
  dontBuild = true;
  dontFixup = true;
  installPhase = ''
    ln -s "/usr" "$out"
  '';
}
