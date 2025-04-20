#!/bin/bash

# Kill any existing geckodriver processes
pkill -f geckodriver || true

# Start geckodriver in the background with logging
geckodriver --log trace > /tmp/geckodriver.log 2>&1 &

# Build the project
cd webdriver-examples
cabal build --enable-tests

# Keep the script running to maintain the container
tail -f /dev/null