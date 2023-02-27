# commong fish settings
{ pkgs, config, specialArgs, ... }: with specialArgs;
let
  windowsDataRoot = "/mnt/d"; # d is for data
  windowsHomeDirectory = "${windowsDataRoot}/Users/kress";
  windowsDesktopDirectory = "${windowsHomeDirectory}/Desktop";
  windowsMainRoot = "/mnt/c"; # c is for "can't move out of here"
  windowsMainDirectory = "${windowsMainRoot}/Windows";

  # binaries
  fish = "${config.programs.fish.package}/bin/fish";
in
{
  programs.fish = {
    shellAliases = {
      # wsl apps 
      "f" = "explorer";
      "neovide" = " /mnt/c/Users/kress/scoop/shims/neovide.exe --wsl";
    };
    shellInit = ''
      set -g SHELL "${fish}"
      set -g w "${windowsDesktopDirectory}/"
      ${(builtins.readFile (fileFromMisc "shell_init.fish"))}
    '';
    functions = {
      shell = ''
        # prefer using direnv as using nix-shell directly is slow
        # also depends on any-nix-shell to maintain current shell
        nix-shell shell.nix $argv[1..-1]
      '';
      develop = ''
        nix develop "/etc/nixos/shells/$argv[1]" -c fish $argv[2..-1];
      '';
      code = ''
        if count $argv >/dev/null
          set here (realpath -m $argv[1])
        else
          set here $PWD
        end
        if test -d $here
          powershell "code --folder-uri=vscode-remote://ssh-remote+localhost$here"
        else
          powershell "code --file-uri=vscode-remote://ssh-remote+localhost$here"
        end
      '';
      explorer = ''
        if count $argv >/dev/null
          set here (realpath -m $argv[1])
        else
          set here $PWD
        end
        # TODO: check if $here is a drive letter
        set here (string replace -a '/' '\\' "//wsl.localhost/NixOS$here")
        ${windowsMainDirectory}/explorer.exe $here
      '';
    };
  };
}
