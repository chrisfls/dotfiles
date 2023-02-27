# base for home-manager only installs
{ pkgs, specialArgs, ... }: with specialArgs;
{
  home.packages = with pkgs; [ 
    agenix.packages."${system}".default
    cachix

    # compression
    p7zip
    unrar
    unzip

    # other
    wget
    jq
    which
  ];
}
