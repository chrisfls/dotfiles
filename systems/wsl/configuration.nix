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

  systemd.services.nixs-wsl-systemd-fix = {
    description = "Fix the /dev/shm symlink to be a mount";
    unitConfig = {
      DefaultDependencies = "no";
      Before = [ "sysinit.target" "systemd-tmpfiles-setup-dev.service" "systemd-tmpfiles-setup.service" "systemd-sysctl.service" ];
      ConditionPathExists = "/dev/shm";
      ConditionPathIsSymbolicLink = "/dev/shm";
      ConditionPathIsMountPoint = "/run/shm";
    };
    serviceConfig = {
      Type = "oneshot";
      ExecStart = [
        "${pkgs.coreutils-full}/bin/rm /dev/shm"
        "/run/wrappers/bin/mount --bind -o X-mount.mkd\ir /run/shm /dev/shm"
      ];
    };
    wantedBy = [ "sysinit.target" ];
  };

  programs.nix-ld.enable = true;

  #
  # services
  #

  services.openssh.passwordAuthentication = false;

  # services.vscode-server = {
  #   enable = true;
  # };

  # before changing this value read the documentation for this option
  system.stateVersion = "22.11";
}
