#!/bin/bash

# Exit immediately if a command exits with a non-zero status
set -e

docker build --tag haskell --file dockerfile-haskell .
docker tag haskell ghcr.io/pyrethrum/haskell:latest
