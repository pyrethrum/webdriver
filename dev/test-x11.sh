#!/bin/bash
# Test X11 connection from inside the container
# Returns 0 if X11 is working, 1 if not

DISPLAY="${DISPLAY:-:0}"

echo "Testing X11 connection..."
echo "  DISPLAY=$DISPLAY"

# Check if DISPLAY is set
if [ -z "$DISPLAY" ]; then
    echo "✗ DISPLAY environment variable is not set"
    exit 1
fi

# Check if X11 socket exists
DISPLAY_NUM=$(echo "$DISPLAY" | sed 's/^://' | sed 's/\..*//')
SOCKET="/tmp/.X11-unix/X${DISPLAY_NUM}"
if [ ! -S "$SOCKET" ]; then
    echo "✗ X11 socket not found: $SOCKET"
    echo "  Available sockets:"
    ls -la /tmp/.X11-unix/ 2>/dev/null || echo "    (none)"
    exit 1
fi
echo "  Socket: $SOCKET exists"

# Check socket permissions
if [ ! -r "$SOCKET" ] || [ ! -w "$SOCKET" ]; then
    echo "✗ Cannot read/write X11 socket"
    echo "  Socket permissions:"
    ls -la "$SOCKET"
    echo "  Current user: $(id)"
    echo ""
    echo "FIX: Run on HOST machine:"
    echo "  xhost +local:"
    exit 1
fi

# Try to connect using xdpyinfo if available
if command -v xdpyinfo &> /dev/null; then
    if xdpyinfo &> /dev/null; then
        echo "✓ X11 connection successful!"
        xdpyinfo | head -5
        exit 0
    else
        echo "✗ Cannot connect to X server"
        echo ""
        echo "FIX: Run on HOST machine:"
        echo "  xhost +local:"
        exit 1
    fi
else
    echo "? xdpyinfo not installed, trying basic socket test..."
    # Try a simple connection test
    if timeout 2 bash -c "echo '' > /dev/tcp/localhost/6000" 2>/dev/null; then
        echo "✓ X11 port appears accessible"
    fi
    echo ""
    echo "To install xdpyinfo for full testing:"
    echo "  sudo apt-get install -y x11-utils"
fi
