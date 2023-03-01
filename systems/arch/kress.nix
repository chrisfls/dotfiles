{ config, pkgs, specialArgs, ... }: with specialArgs;
let
  fish = "${config.programs.fish.package}/bin/fish";
in
{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "kress";
  home.homeDirectory = "/home/kress";

  home.file = {
    ".profile".text = ''
      export PATH=$HOME/.dotnet/tools:$HOME/.local/bin:$PATH
      if [[ $(ps --no-header --pid=$PPID --format=comm) != "fish" && -z "$BASH_EXECUTION_STRING" ]]
      then
        exec ${fish}
      fi
    '';
    "gitlab/.keep".text = "";
  };

  # let home manager install and manage itself
  programs.home-manager.enable = true;
  
  # before changing this value read the documentation for this option
  home.stateVersion = "22.11";
}