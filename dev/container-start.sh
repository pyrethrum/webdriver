#!/bin/bash

# Exit immediately if a command exits with a non-zero status
set -e

# Kill any existing geckodriver processes
bash ./dev/start-geckodriver.sh

# Build the project
cabal build all
