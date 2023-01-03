{ pkgs, ... }:
with pkgs;
[
  bashInteractive
  coreutils-full
  curl
  gawk
  gnugrep
  openssl
  openssh
]
