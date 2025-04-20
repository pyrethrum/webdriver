#!/bin/bash

# Exit immediately if a command exits with a non-zero status
set -e

# Kill any existing geckodriver processes
sh ./dev/start-geckodriver.sh

# Build the project
cd webdriver-examples
cabal build --enable-tests
