#!/usr/bin/bash

if [ -f "./data/git/forgejo.zip" ]; then
  docker-compose run -u git server rm /data/git/forgejo.zip
fi

docker-compose run -u git server "/usr/local/bin/forgejo" dump --file "/data/git/forgejo.zip"
docker-compose cp server:/data/git/forgejo.zip forgejo.zip
docker-compose run -u git server rm /data/git/forgejo.zip
rclone copyto forgejo.zip cflsousa:backups/forgejo.zip -P
