#!/bin/bash
echo "Creating a new session..."
SESSION_RESPONSE=$(curl -s -X POST http://127.0.0.1:4444/session -H "Content-Type: application/json" -d '{"capabilities":{"alwaysMatch":{"acceptInsecureCerts":true,"browserName":"firefox"}}}')
SESSION_ID=$(echo $SESSION_RESPONSE | grep -o '"sessionId":"[^"]*' | cut -d'"' -f4)

if [ -z "$SESSION_ID" ]; then
  echo "Failed to create session"
  exit 1
fi

echo "Session created with ID: $SESSION_ID"
echo "Attempting to connect to BiDi endpoint..."

# Try different WebSocket paths
echo "Testing WebSocket connection to /session/$SESSION_ID/webdriver-bidi"
"$BROWSER" "http://127.0.0.1:4444/session/$SESSION_ID/webdriver-bidi"

echo "You can also try these paths:"
echo "http://127.0.0.1:4444/session/$SESSION_ID"
echo "http://127.0.0.1:4444/session/webdriver-bidi"