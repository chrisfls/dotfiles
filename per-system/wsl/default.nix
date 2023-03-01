{ config, pkgs, specialArgs, ... }:
with specialArgs;
{
  imports = [
    ../../nixos
    ../../nixos/cloudflare-warp.nix
  ];

  networking = { hostName = "wsl"; };

  security.sudo.wheelNeedsPassword = false;

  wsl = {
    enable = true;
    defaultUser = "kress";
    wslConf.automount.root = "/mnt";
    startMenuLaunchers = true;
    nativeSystemd = true;
    interop.register = false;
    interop.includePath = false;
  };

  # workaround to fix nix-wsl
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
  # virtualisation / containerization
  #

  boot.enableContainers = false; # conflicts with virtualisation.containers

  virtualisation.podman = {
    enable = true;
    dockerCompat = true; # podman provides docker
  };

  #
  # programs (that require more than their packages to work)
  #

  programs.command-not-found.enable = false;

  #
  # services
  #

  services.openssh.settings.PasswordAuthentication = false;

  services.cloudflare-warp = {
    enable = true;
    # not working, for some reason:
    # # download from https://developers.cloudflare.com/cloudflare-one/static/documentation/connections/Cloudflare_CA.crt
    # certificate = fileFromMisc "Cloudflare_CA.crt";
  };

  #
  # users
  #

  users.users.root.openssh.authorizedKeys.keys = [
    keys."KGPDWM/kress"
  ];

  users.users.${config.wsl.defaultUser} = {
    hashedPassword = "$6$tKkm8UV4qq7GrjqA$PvAGAV0g/8bgEpWoIBoUcYhBHzq.3bTUxzWH./p8lFUpfndlCheJ2h.LYg8s3R.0Mhawdi.O09rNH2GOZ88od1";
    openssh.authorizedKeys.keys = [
      keys."KGPDWM/kress"
    ];
  };

  users.groups.docker.members = [ config.wsl.defaultUser ];

  home-manager.users.${config.wsl.defaultUser} = {
    imports = [
      homeage.homeManagerModules.homeage
      ./per-user/${config.wsl.defaultUser}
    ];
  };

  # before changing this value read the documentation for this option
  system.stateVersion = "22.11";
}
