#!/bin/bash

# Exit immediately if a command exits with a non-zero status
set -e

docker build --tag "haskell-webdriver" --file DockerfileHaskell-WebDriver .
# docker tag haskell theghostjw/haskell:latest
# docker push theghostjw/haskell:latest