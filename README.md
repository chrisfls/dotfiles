```sh
git clone --recurse-submodules git@github.com:kress95/nix-configs.git /etc/nixos
```

### Structure

(not implemented yet)

- `assets/**/*`
    - Generic assets required `configuration.nix` and `home.nix`.
- `modules/{{module}}.nix`
    - Generic modules to be imported from the `configuration.nix`.
- `pkgs/{{package}}.nix`
    - Packages that aren't packaged (or properly packaged) in the official repos.
- `secrets/`
    - Submodule with encrypted sensible data.
- `secrets/keys/{{hostname}}/{{username}}_id_{{algorithm}}.pub`
    - Public keys.
- `secrets/{{hostname}}/{{username}}/**/*`
    - User-platform specific encrypted assets required by `home.nix` modules.
- `shells/{{shell}}.nix`
    - `shell.nix` that for some reason can't be included in their projects.
- `systems/{{hostname}}.nix`
    - `configuration.nix` for each managed _NixOS_ system.
- `systems/{{hostname}}/{{username}}.nix`
    - `home.nix` home manager configuration for each user of the system.
- `systems/{{hostname}}/{{username}}/**/*`
    - User-platform specific assets required by `home.nix` modules.
- `systems/{{hostname}}/assets/**/*`
    - Platform specific assets required by `home.nix` modules.
- `home/{{module}}.nix`
    - Generic home manager modules to be imported from `home.nix`.
- `home/{{module}}/**/*`
    - Assets required by `user/*.nix` modules.
