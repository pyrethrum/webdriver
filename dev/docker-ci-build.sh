#!/bin/bash

# Exit immediately if a command exits with a non-zero status
set -e

# NOTE: Run from webdriver repo root
docker build --tag webdriver --file Dockerfile .
