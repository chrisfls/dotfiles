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
  # packages
  #

  environment.systemPackages = with pkgs; [
    code-server
  ];

  #
  # services
  #

  services.openssh.permitRootLogin = "yes";
  
  services.code-server = { # ssh -N -L 42069:127.0.0.1:42069 root@wsl
    enable = true;
    auth = "none";
    host = "0.0.0.0";
    user = "kress";
    port = 42069;
  };

  # services.vscode-server = {
  #   enable = true;
  # };

  # before changing this value read the documentation for this option
  system.stateVersion = "22.11";
}
