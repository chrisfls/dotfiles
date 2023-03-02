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

  #
  # packages
  #

  nixpkgs.config.allowUnfree = true;

  environment.systemPackages = with pkgs; [
    agenix.packages."${system}".default # secrets
    cachix # cache

    # shell
    any-nix-shell
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
  # services
  #

  services.timesyncd.enable = true;

  services.openssh = {
    enable = true;
    settings = {
      passwordAuthentication = false;
      kbdInteractiveAuthentication = false;
    };
  };

  #
  # default home-manager config
  #

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    extraSpecialArgs = specialArgs // {
      distro = "NixOS";
      nixosConfig = {
        networking = { hostName = config.networking.hostName; };
      };
    };
  };
}
