#!/bin/sh
# A chromedriver stand-in for run.sh that never answers, so the gate's 15s wait
# for it to be ready ends BLOCKED.
exec sleep 600
