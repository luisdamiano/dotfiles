#!/bin/bash

# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

# Launch Polybar, using default config location ~/.config/polybar/config
polybar topcenter -r 2>~/.config/polybar/log &
polybar bottomleft -r 2>~/.config/polybar/log &
polybar bottomcenter -r 2>~/.config/polybar/log &
polybar bottomright -r 2>~/.config/polybar/log &

echo "Polybar launched..."
