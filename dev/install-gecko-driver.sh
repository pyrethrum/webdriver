#!/bin/bash

# Exit immediately if a command exits with a non-zero status
set -e

# Variables
# FIREFOX_URL="https://download.mozilla.org/?product=firefox-latest&os=linux64&lang=en-US"
GECKODRIVER_VERSION="v0.35.0"
GECKODRIVER_URL="https://github.com/mozilla/geckodriver/releases/download/$GECKODRIVER_VERSION/geckodriver-$GECKODRIVER_VERSION-linux64.tar.gz"

# Install dependencies
sudo apt-get update
sudo apt-get install -y wget tar


# Download and install GeckoDriver
wget $GECKODRIVER_URL -O /tmp/geckodriver.tar.gz
tar -xzf /tmp/geckodriver.tar.gz -C /tmp
sudo mv /tmp/geckodriver /usr/local/bin/
sudo chown vscode:vscode /usr/local/bin/geckodriver
sudo chmod +x /usr/local/bin/geckodriver

# Clean up
rm /tmp/geckodriver.tar.gz

# Verify installation
echo "GeckoDriver version:"
geckodriver --version


echo "Installation complete. GeckoDriver installed and can be run by the user 'vscode'."