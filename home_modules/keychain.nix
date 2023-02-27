{ pkgs, specialArgs, ... }: with specialArgs;
{
  programs.keychain = {
    enable = true;
    keys = [ "id_ed25519" ];
  };
}