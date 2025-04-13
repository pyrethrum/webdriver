#!/bin/bash

# Exit immediately if a command exits with a non-zero status
set -e

# needs execute permissions
# may need to run chmod +x DockerEverything.sh DockerBuildPushHaskell.sh DockerBuildPushPyrethrum.sh

# Run bs1.sh
./DockerBuildPushHaskell.sh && \
./DockerBuildPushPyrethrum.sh