#!/bin/bash

# Exit immediately if a command exits with a non-zero status
set -e

#  need to be logged into theghostjw account on docker hub
docker pull theghostjw/haskell:latest
docker build -t "pyrethrum" -f DockerfilePyrethrum .
docker tag pyrethrum theghostjw/pyrethrum:latest
docker push theghostjw/pyrethrum:latest