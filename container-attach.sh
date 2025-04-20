#!/bin/bash
cd webdriver-examples
cabal build --enable-tests
pkill -f geckodriver || true
echo "Starting geckodriver..."
geckodriver &