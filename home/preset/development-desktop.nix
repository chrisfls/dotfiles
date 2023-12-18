{ config, inputs, pkgs, ... }:
{
  imports = [ ../module ];

  extra.codium.enable = true;
}
