# commong fish settings
{ pkgs, ... }:
{
  programs.fish.shellAliases = {
    "rebuild-home" = "eval (cat /etc/systemd/system/home-manager-$USER.service | sed -n 's/ExecStart=//p')";
    "rebuild-sys" = "sudo nixos-rebuild switch -v && rebuild-home -v";
  };
}
