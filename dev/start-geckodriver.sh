#!/bin/bash

# Exit immediately if a command exits with a non-zero status
set -e

# Kill any existing geckodriver processes
pkill -x geckodriver || true

PORT=4444
# Start geckodriver in the background with logging
nohup geckodriver --port $PORT > /dev/null 2>&1 &
disown
echo "geckodriver started with PID $! at http://127.0.0.1:$PORT"

# Wait to ensure geckodriver is up and running and detached (neeeded when running from a task)
sleep 1