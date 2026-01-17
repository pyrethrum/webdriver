#!/bin/bash
# This runs when the terminal attaches
echo "Dev container ready to run examples."

# Quick X11 check (non-blocking)
if [ -n "$DISPLAY" ]; then
    DISPLAY_NUM=$(echo "$DISPLAY" | sed 's/^://' | sed 's/\..*//')
    SOCKET="/tmp/.X11-unix/X${DISPLAY_NUM}"
    if [ -S "$SOCKET" ] && [ ! -r "$SOCKET" ]; then
        echo ""
        echo "⚠️  X11 Warning: Cannot access display socket"
        echo "   Headed browser tests will fail."
        echo "   Run on HOST: xhost +local:"
        echo "   Or run: bash ./dev/test-x11.sh for details"
    fi
fi
