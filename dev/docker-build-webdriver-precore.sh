#!/bin/bash

# Exit immediately if a command exits with a non-zero status
set -e

docker build --tag webdriver-precore --file dockerfile-webdriver-precore .
docker tag webdriver-precore ghcr.io/pyrethrum/webdriver-precore:latest
