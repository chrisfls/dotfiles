{ config, pkgs, nixpkgs, specialArgs, ... }: with specialArgs;
{
  networking = {
    hostName = "wsl";
  };

  wsl = {
    enable = true;
    wslConf.automount.root = "/mnt";
    defaultUser = defaultUser;
    startMenuLaunchers = true;
    nativeSystemd = true;
    interop.register = false;
    interop.includePath = false;
  };

  #
  # services
  #

  services.openssh.permitRootLogin = "yes";

  services.vscode-server = {
    enable = true;
  };

  # before changing this value read the documentation for this option
  system.stateVersion = "22.11";
}
