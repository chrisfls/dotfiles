#!/usr/bin/bash

if [ -f "./forgejo.zip" ]; then
  rm forgejo.zip
fi

rclone copyto cflsousa:backups/forgejo.zip ./forgejo.zip -P
docker-compose cp ./forgejo.zip server:/data/forgejo.zip
docker-compose run server rm -rf /data/dump
docker-compose run server unzip /data/forgejo.zip -d /data/dump

# mv data/* /data/gitea
docker-compose run server rm -rf /data/gitea
docker-compose run server bash -ec 'set -e; find /data/dump/data -mindepth 1 -maxdepth 1 -exec mv -t /data/gitea/ {} +; exit'

# mv repos/* /data/git/gitea-repositories/
docker-compose run server rm -rf /data/git/gitea-repositories
docker-compose run server bash -ec 'set -e; find /data/dump/repos -mindepth 1 -maxdepth 1 -exec mv -t /data/git/gitea-repositories/ {} +; exit'


#/usr/local/bin/gitea -c '/data/gitea/conf/app.ini' admin regenerate hooks
# /data/gitea/gitea.db
