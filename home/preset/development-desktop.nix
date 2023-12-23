{ config, inputs, pkgs, ... }:
{
  imports = [ ../module ];
  module.codium.enable = true;
}
