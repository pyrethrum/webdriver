#!/bin/bash

# Exit immediately if a command exits with a non-zero status
set -e

# Kill any existing geckodriver processes
pkill -x chromedriver || true


PORT=4444

  # --allowed-ips="" \
nohup chromedriver \
  --port=$PORT \
  > chromedriver.log 2>&1 &

CHROMEDRIVER_PID=$!
echo "chromedriver started with PID $CHROMEDRIVER_PID at http://127.0.0.1:$PORT"

# Wait for chromedriver to be ready before script exits
echo "Waiting for chromedriver..."
for i in {1..50}; do
  if (bash -c ">/dev/tcp/127.0.0.1/$PORT" 2>/dev/null); then
    echo "Port $PORT is open"
    break
  fi
  sleep 0.1
  if [ $i -eq 50 ]; then
    echo "Timeout: Port $PORT failed to open"
    exit 1
  fi
done

disown
# wait_for_port $WEBSOCKET_PORT
# websocket port 0 (dynamic) will always fail)