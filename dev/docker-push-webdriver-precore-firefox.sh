#!/bin/bash

# Exit immediately if a command exits with a non-zero status
set -e

# Prompt for username and API key if not provided as parameters or environment variables
if [ -z "$USERNAME" ] || [ -z "$AUTH_TOKEN" ]; then
  if [ "$#" -ne 2 ]; then
    echo "This script uploads the webdriver-precore-firefox image to GitHub Container Registry."
    echo "It requires your GitHub username and API key."
    read -p "Enter your GitHub username: " USERNAME
    read -sp "Enter your GITHUB Auth Token: " AUTH_TOKEN
    echo
  else
    USERNAME="$1"
    AUTH_TOKEN="$2"
  fi
fi

# Log in to GitHub Container Registry
echo "$AUTH_TOKEN" | docker login ghcr.io -u "$USERNAME" --password-stdin

# Push the Docker image
docker push ghcr.io/pyrethrum/webdriver-precore-firefox:latest
