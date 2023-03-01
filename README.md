```sh
git clone --recurse-submodules git@github.com:kress95/nix-configs.git /etc/nixos
```

### Structure

(not implemented yet)

- [x] `assets/**/*`
    - Generic assets required `configuration.nix` and `home.nix`.
- [x] `home/{{module}}.nix` `home/{{module}}/default.nix`
    - Generic home manager modules to be imported from `home.nix`.
- [x] `home/{{module}}/**/*`
    - Assets required by `user/*.nix` modules.
- [x] `host/{{module}}.nix` `host/{{module}}/default.nix` 
    - Generic modules to be imported from the `configuration.nix`.
- [x] `pkgs/{{package}}.nix` `pkgs/{{package}}/default.nix`
    - Packages that aren't packaged (or properly packaged) in the official repos.
- [x] `secrets/`
    - Submodule with encrypted sensible data.
- [ ] `secrets/keys/{{hostname}}/{{username}}_id_{{algorithm}}.pub`
    - Public keys.
- [ ] `secrets/{{hostname}}/{{username}}/**/*`
    - User-platform specific encrypted assets required by `home.nix` modules.
- [x] `shells/{{shell}}.nix` `shells/{{shell}}/default.nix`
    - `shell.nix` that for some reason can't be included in their projects.
- [x] `per-system/{{hostname}}/default.nix`
    - `configuration.nix` for each managed _NixOS_ system.
        - Might also include `hardware-configuration.nix`, but I only have WSL systems.
- [x] `per-system/{{hostname}}/per-user/{{username}}/default.nix`
    - `home.nix` home manager configuration for each user of the system.
- [x] `per-system/{{hostname}}/per-user/{{username}}/**/*`
    - User-platform specific assets required by `home.nix` modules.
- [x] `per-system/{{hostname}}/assets/**/*` (not used yet)
    - Platform specific assets required by `configuration.nix` modules.
