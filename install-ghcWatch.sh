#!/bin/bash

# needs to be made executable: chmod +x install-ghcWatch.sh

# Exit immediately if a command exits with a non-zero status
set -e

# Variables
GHC_WATCH_URL="wget https://github.com/MercuryTechnologies/ghciwatch/releases/download/v1.0.1/ghciwatch-x86_64-linux -O /usr/local/bin/ghciwatch"

wget $GHC_WATCH_URL -O /usr/local/bin/ghciwatch
sudo chmod +x /usr/local/bin/ghciwatch
ghciwatch --version