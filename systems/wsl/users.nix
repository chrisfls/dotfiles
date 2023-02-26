{ specialArgs, ... }: with specialArgs;
{
  users.users.root.openssh.authorizedKeys.keys = [
    keys."KGPDWM/kress"
  ];

  users.users.${defaultUser} = {
    hashedPassword = "$6$tKkm8UV4qq7GrjqA$PvAGAV0g/8bgEpWoIBoUcYhBHzq.3bTUxzWH./p8lFUpfndlCheJ2h.LYg8s3R.0Mhawdi.O09rNH2GOZ88od1";
    openssh.authorizedKeys.keys = [
      keys."KGPDWM/kress"
    ];
  };

  users.groups.docker.members = [ defaultUser ];

  home-manager.users.${defaultUser} = importUser defaultUser "wsl";
}
