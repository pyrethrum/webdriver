# BiDi Connection Failure

## GeckoDriver Start Script

```bash
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
```