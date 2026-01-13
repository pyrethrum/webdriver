#!/bin/bash

# Exit immediately if a command exits with a non-zero status
set -e

docker build --tag webdriver-precore-firefox --file dockerfile-webdriver-precore-firefox .
docker tag webdriver-precore-firefox ghcr.io/pyrethrum/webdriver-precore-firefox:latest
