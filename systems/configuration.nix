{ config, pkgs, lib, nixpkgs, specialArgs, ... }: with specialArgs;
{
  nix = {
    package = pkgs.nixUnstable;

    extraOptions = ''
      experimental-features = nix-command flakes
    '';

    # reuse flakes inputs for old commands
    nixPath = [ "nixpkgs=${nixpkgs}" ];

    # always uses system's flakes instead of downloading or updating
    registry.nixpkgs.flake = nixpkgs;
  };

  imports = [ ../pkgs/cloudflare-warp.nix ];

  networking = {
    firewall.enable = false;
    usePredictableInterfaceNames = true;
  };

  time.timeZone = "America/Sao_Paulo";

  i18n = {
    defaultLocale = "en_GB.UTF-8";
    supportedLocales = [ "en_GB.UTF-8/UTF-8" "en_US.UTF-8/UTF-8" "pt_BR.UTF-8/UTF-8" ];
    # broken, fix later
    # extraLocaleSettings = {
    #   LC_TIME = "pt_BR.UTF-8/UTF-8";
    # };
  };

  security.sudo.wheelNeedsPassword = false;

  #
  # virtualisation / containerization
  #

  boot.enableContainers = false; # conflicts with virtualisation.containers

  virtualisation.podman = {
    enable = true;
    dockerCompat = true; # podman provides docker
  };

  #
  # packages
  #

  nixpkgs.config.allowUnfree = true;

  environment.systemPackages = with pkgs; [
    agenix.defaultPackage."${system}" # secrets
    cachix # cache

    # shell
    fish
    fishPlugins.autopair-fish # probably not needed
    fishPlugins.colored-man-pages
    fishPlugins.done # probably never used
    fishPlugins.foreign-env # probably not needed
    fishPlugins.sponge
    fishPlugins.tide

    # git
    git
    micro

    # compression
    p7zip
    unrar
    unzip

    # tooling
    htop
    traceroute
    killall
    neofetch
    mosh # probably never used

    # used to support languagetool in vscode
    adoptopenjdk-jre-openj9-bin-16

    # used just to setup cloudflare warp
    brave
    xdg-utils
    desktop-file-utils

    # other
    wget
    jq
    nixpkgs-fmt
  ];

  environment.shells = with pkgs; [ fish ];

  #
  # default env
  #

  environment.variables = {
    EDITOR = "micro";
    TERM = "xterm-256color";
    COLORTERM = "truecolor";
    MICRO_TRUECOLOR = "1";
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

  #
  # services
  #

  services.timesyncd.enable = true;

  services.openssh = {
    enable = true;
    passwordAuthentication = false;
    kbdInteractiveAuthentication = false;
  };

  services.cloudflare-warp = {
    enable = true;
    # not working, for some reason:
    # # download from https://developers.cloudflare.com/cloudflare-one/static/documentation/connections/Cloudflare_CA.crt
    # certificate = fileFromMisc "Cloudflare_CA.crt";
  };

  #
  # default home-manager config
  #

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    extraSpecialArgs = specialArgs;
  };
}
