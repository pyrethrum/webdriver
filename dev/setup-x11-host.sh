#!/bin/bash
# Run this script on the HOST machine (not inside the container)
# before opening the dev container.
#
# This enables X11 forwarding for the dev container to display
# headed browser windows.
#
# Usage: ./dev/setup-x11-host.sh

echo "Setting up X11 access for Docker containers..."

# Method 1: Allow all local connections (most compatible)
xhost +local: 2>/dev/null && echo "✓ Enabled local X11 access"

# Alternative: More restrictive - only local user
# xhost +SI:localuser:$(whoami)

echo ""
echo "X11 access configured. You can now open the dev container."
echo ""
echo "To verify after container starts, run inside container:"
echo "  xdpyinfo | head -5"
