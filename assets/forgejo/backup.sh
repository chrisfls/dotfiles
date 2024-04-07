#!/usr/bin/bash

docker-compose run server tar -czvf "/data/forgejo.tar.gz" "/data/git" "/data/gitea" "/data/ssh"
rclone copyto ./data/forgejo.tar.gz cflsousa:backups/forgejo.tar.gz -P
docker-compose run server rm /data/forgejo.tar.gz
