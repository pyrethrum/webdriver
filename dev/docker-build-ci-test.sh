#!/bin/bash

# Exit immediately if a command exits with a non-zero status
set -e

# Build from the parent webdriver directory
cd "$(dirname "$0")/.."
docker build --progress=plain --tag ci-test --file dev/dockerfile-ci-test .
docker tag ci-test ghcr.io/pyrethrum/ci-test:latest
