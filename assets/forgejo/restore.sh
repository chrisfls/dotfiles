#!/usr/bin/bash

rclone copyto cflsousa:backups/forgejo.tar.gz ./data/forgejo.tar.gz -P
docker-compose run server rm -rf "/data/git" "/data/gitea" "/data/ssh"
docker-compose run server tar -xzvf /data/forgejo.tar.gz -C /
docker-compose run server rm /data/forgejo.tar.gz
