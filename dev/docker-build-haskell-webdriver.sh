#!/bin/bash

# Exit immediately if a command exits with a non-zero status
set -e

docker build --tag haskell-webdriver --file dockerfile-haskell-webdriver .
docker tag haskell-webdriver haskell-webdriver:latest
