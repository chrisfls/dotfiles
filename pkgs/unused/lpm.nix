{ lib, stdenv, fetchurl, ... }:
stdenv.mkDerivation {
  name = "lite-xl-plugin-manager";

  dontUnpack = true;

  src = fetchurl {
    url = "https://github.com/lite-xl/lite-xl-plugin-manager/releases/download/latest/lpm.x86_64-linux";
    sha256 = "sha256-SWqtrSyGKIH3GVW+mnlchTUzjyip7Z2jUR1zgarUxCc=";
  };

  installPhase = ''
    mkdir -p $out/bin
    cp $src $out/bin/lpm
    chmod +x $out/bin/lpm
  '';

  meta = with lib; {
    description = "Lite XL Plugin Manager";
    license = licenses.mit;
  };
}
