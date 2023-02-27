# common git settings
{ pkgs, specialArgs, ... }: with specialArgs;
let
  homeDirectory = "/home/kress";
in
{
  home.packages = with pkgs; [
    micro
  ];

  programs.git = {
    enable = true;
    userName = "Chris";
    userEmail = "2013206+kress95@users.noreply.github.com";
    extraConfig = {
      rerere.enabled = true;
      pull.rebase = true;
      init.defaultBranch = "main";
      core = {
        editor = "micro";
        excludesfile = "$NIXOS_CONFIG_DIR/scripts/gitignore";
      };
    };
    includes = [
      {
        condition = "gitdir:${homeDirectory}/gitlab/";
        contents.user.email = "664520-kress95@users.noreply.gitlab.com";
      }
      {
        condition = "gitdir:${homeDirectory}/paack/";
        contents = {
          user.name = "Christian Ferraz";
          user.email = "christian.ferraz@paack.co";
        };
      }
    ];
  };
}
