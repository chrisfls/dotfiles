{ config, pkgs, nixpkgs, specialArgs, ... }: with specialArgs;
{
  imports = [
    nixos-wsl.nixosModules.wsl
    home-manager.nixosModules.home-manager
    ../modules/configuration.nix
    ../pkgs/cloudflare-warp.nix
  ];

  networking = {
    hostName = "wsl";
  };

  security.sudo.wheelNeedsPassword = false;

  wsl = {
    enable = true;
    wslConf.automount.root = "/mnt";
    defaultUser = defaultUser;
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

  programs.fish = {
    enable = true;
    vendor = {
      config.enable = true;
      completions.enable = true;
      functions.enable = true;
    };
  };

  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
    defaultEditor = false;
  };

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

  users.users.${defaultUser} = {
    hashedPassword = "$6$tKkm8UV4qq7GrjqA$PvAGAV0g/8bgEpWoIBoUcYhBHzq.3bTUxzWH./p8lFUpfndlCheJ2h.LYg8s3R.0Mhawdi.O09rNH2GOZ88od1";
    openssh.authorizedKeys.keys = [
      keys."KGPDWM/kress"
    ];
  };

  users.groups.docker.members = [ defaultUser ];

  home-manager.users.${defaultUser} = importUser defaultUser "wsl";

  # before changing this value read the documentation for this option
  system.stateVersion = "22.11";
}
