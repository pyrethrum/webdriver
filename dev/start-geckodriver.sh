#!/bin/bash

# Exit immediately if a command exits with a non-zero status
set -e

# Kill any existing geckodriver processes
pkill -x geckodriver || true

PORT=4444
WEBSOCKET_PORT=9222
MARIONETTE_PORT=2828

# Set Firefox preferences
# export MOZ_REMOTE_SETTINGS_DEV=1
# export MOZ_ENABLE_WEBDRIVER=1

# nohup geckodriver \
#   --port $PORT \
#   --websocket-port $WEBSOCKET_PORT \
#   --binary "$(which firefox)" \
#   --marionette-port $MARIONETTE_PORT > geckodriver.log 2>&1 &

nohup geckodriver \
  --port $PORT \
  --websocket-port $WEBSOCKET_PORT > geckodriver.log 2>&1 &

disown
echo "geckodriver started with PID $! at http://127.0.0.1:$PORT"

# Wait to ensure geckodriver is up and running and detached (neeeded when running from a task)
sleep 5


# Port checking alternatives
echo "Checking ports..."
check_port() {
  (timeout 1 bash -c '</dev/tcp/127.0.0.1/'$1) 2>/dev/null && \
    echo "Port $1 is open" || \
    echo "Port $1 failed"
}

check_port $PORT
check_port $WEBSOCKET_PORT