#!/bin/bash

# Exit immediately if a command exits with a non-zero status
set -e

# Kill any existing geckodriver processes
pkill -x geckodriver || true

PORT=4444
# 0 means a random port will be assigned
WEBSOCKET_PORT=0 
MARIONETTE_PORT=2828

nohup geckodriver \
  --port $PORT \
  --websocket-port $WEBSOCKET_PORT > geckodriver.log 2>&1 &

disown
echo "geckodriver started with PID $! at http://127.0.0.1:$PORT"

# Wait for geckodriver to be ready before script exits
echo "Checking port..."
wait_for_port() {
  (timeout 10 bash -c '</dev/tcp/127.0.0.1/'$1) 2>/dev/null && \
    echo "Port $1 is open" || \
    echo "Port $1 failed"
}

wait_for_port $PORT
# wait_for_port $WEBSOCKET_PORT
# websocket port 0 (dynamic) will always fail)