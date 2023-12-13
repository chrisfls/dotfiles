rec {
  hosts = {
    arch-rmxp = {
      hostname = "arch-rmxp";
    };
  };
  users = {
    arch-rmxp = {
      kress = {
        username = contact.home;
        id = "${users.arch-rmxp.kress.username}@${hosts.arch-rmxp.hostname}";
        home = "/home/${users.arch-rmxp.kress.username}";
      };
    };
  };
  keys = {
    # TODO: cleanup/invalidate arch-wsl-rpgmxp keys
    arch-rmxp = {
      kress = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFTUCRveiVk6Ri/F5B6KTNmyYFVNmlxNOCwIscuwZCvt kress@arch-rpgmxp";
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
    nickname = "kress95";
    home = "kress";
    name = "Chris";
    email = "2013206+kress95@users.noreply.github.com";
    gitlab = {
      email = "664520-kress95@users.noreply.gitlab.com";
    };
    work = {
      name = contact.name;
      email = contact.email;
    };
  };
}
