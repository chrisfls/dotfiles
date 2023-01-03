This doesn't actually work yet...

### build

```sh
nix-build ./${SHELL_NAME}/docker-image.nix
```

### import

```sh
docker image import /nix/store/${HASH}-docker-image-${SHELL_NAME}.tar.gz
```

### run

```sh
docker run -it --network=host ${IMAGE_ID}
```
