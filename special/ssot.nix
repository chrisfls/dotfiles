rec {
  hosts = {
    arch-rmxp = {
      hostname = "arch-rmxp";
    };
  };
  users = {
    arch-rmxp = {
      chris = {
        username = contact.home;
        id = "${users.arch-rmxp.chris.username}@${hosts.arch-rmxp.hostname}";
        home = "/home/${users.arch-rmxp.chris.username}";
      };
    };
  };
  keys = {
    # TODO: cleanup/invalidate arch-wsl-rpgmxp keys
    arch-rmxp = {
      chris = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFTUCRveiVk6Ri/F5B6KTNmyYFVNmlxNOCwIscuwZCvt kress@arch-rpgmxp";
    };
    arch-wsl-rpgmxp = {
      kress = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFTUCRveiVk6Ri/F5B6KTNmyYFVNmlxNOCwIscuwZCvt kress@arch-wsl-rpgmxp";
    };
    wsl = {
      root = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINAewif8xDsvADjpZlcH9qaQyQzJurtUG4SQIzbwrUch root@wsl";
      kress = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAVEIFN8oQFCOAC4gqjnvsLimaMS8Ldtx4ck+XihmVxl kress@wsl";
    };
    KGPDWM = {
      kress = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICz2lPtbzhC3L0Z4CCfkyZlkUI7Gc62P0iBjay/fzbVx cferraz95@gmail.com";
    };
  };
  contact = {
    nickname = "chrisfls";
    home = "chris";
    name = "Chris";
    email = "2013206+chrisfls@users.noreply.github.com";
    work = {
      name = "Christian";
      email = "2013206+chrisfls@users.noreply.github.com";
    };
    github = {
      email = "160303871+kress-x@users.noreply.github.com";
    };
    gitlab = {
      # TODO: rename to kress-x
      email = "664520-kress95@users.noreply.gitlab.com";
    };
  };
}
