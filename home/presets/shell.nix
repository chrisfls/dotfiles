{ config, lib, pkgs, ... }:
{
  modules = {
    agenix.enable = true;
    bash.enable = true;
    direnv.enable = true;
    keychain.enable = true;
    micro.enable = true;
    zoxide.enable = true;
  };
}
