#!/bin/bash

# Exit immediately if a command exits with a non-zero status
set -e

docker build --tag ghcr.io/pyrethrum/haskell:latest --file dockerfile-haskell .