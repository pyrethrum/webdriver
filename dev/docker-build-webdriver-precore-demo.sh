#!/bin/bash

# Exit immediately if a command exits with a non-zero status
set -e

docker build --tag webdriver-precore-demo --file dockerfile-webdriver-precore-demo .
docker tag webdriver-precore-demo ghcr.io/pyrethrum/webdriver-precore-demo:latest
