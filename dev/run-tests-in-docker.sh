#!/bin/bash

# Exit immediately if a command exits with a non-zero status
set -e

IMAGE=${IMAGE:-ci-test}

echo "Building Docker image: ${IMAGE}"
cd "$(dirname "$0")/.."
docker build --tag ${IMAGE} --file dev/dockerfile-ci-test .

echo "Running tests in Docker container"
docker run ${IMAGE} bash -c \
  'bash ./dev/start-geckodriver.sh & cabal test all'
  # 'geckodriver --version && firefox --version && bash ./dev/start-geckodriver.sh & cabal test all --test-options="-p \"/Input Events - File Dialog Opened (Single File) - EXPECTED ERROR: input.fileDialogOpened is not a valid event name/\""'
