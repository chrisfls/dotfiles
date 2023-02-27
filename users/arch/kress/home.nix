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
    ".bashrc".source = fileFromHome ".bashrc";
    ".bash_profile".source = fileFromHome ".bash_profile";
    ".profile".text = ''
      if [[ $(ps --no-header --pid=$PPID --format=comm) != "fish" && -z "$BASH_EXECUTION_STRING" ]]
      then
        exec ${fish}
      fi
    '';
    "gitlab/.keep".text = "";
    "paack/.envrc".source = fileFromMisc "/paack/.envrc";
    ".local/bin/cmd".source = fileFromMisc "cmd";
    ".local/bin/powershell".source = fileFromMisc "powershell";
  };

  # let home manager install and manage itself
  programs.home-manager.enable = true;
  
  # before changing this value read the documentation for this option
  home.stateVersion = "22.11";
}