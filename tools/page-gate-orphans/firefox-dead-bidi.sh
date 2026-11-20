#!/bin/sh
# A Firefox stand-in for run.sh: once a browser has loaded the page of the site
# ORPHAN_SITE names, it prints a BiDi endpoint nothing listens on, so the gate's
# Firefox connection fails and the gate ends BLOCKED, and then it stays up, as a
# browser would.
i=0
while [ ! -s "$ORPHAN_SITE/navigated" ] && [ $i -lt 600 ]; do
  sleep 0.1
  i=$((i + 1))
done
sleep 1
echo "WebDriver BiDi listening on ws://127.0.0.1:9"
exec sleep 600
