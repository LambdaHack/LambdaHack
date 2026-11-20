#!/bin/bash
# The case of the page gate's orphaned browsers (defect page-gate-06 in
# tools/defects.json): the gate ends while a browser it started is up,
# and no process of that run may be left once it has. Each variant runs
# ts-src/page-gate.mjs over a site of its own, a temporary directory whose
# serve.mjs, this directory's, serves a blank page and notes each load:
#   firefox-blocked   Chromium up, Firefox's gate BLOCKED: a Firefox stand-in,
#                     firefox-dead-bidi.sh, prints a dead BiDi endpoint once
#                     Chromium has loaded the page
#   chromium-blocked  Firefox up, Chromium's gate BLOCKED: a chromedriver
#                     stand-in, chromedriver-mute.sh, never answers
#   sigint            both up, the gate sent SIGINT once both loaded the page
#   sigterm           both up, the gate sent SIGTERM likewise
#
# Usage: tools/page-gate-orphans/run.sh [VARIANT...], every variant by default,
# one after another, the gate's ports being fixed, and never beside a page
# gate. Run it unsandboxed, as `make page-gate` is run, Chromium not starting
# sandboxed. It needs ~/.ghc-wasm/env's node, ts-src's packages for the gate's
# bundle of the palette and the browsers the gate runs, and no build or deployed
# site.
#
# A process of the run is one that inherited ORPHAN_SITE, set for the gate
# alone, or names the run's site directory in its argv, as every Chromium
# process does by its --user-data-dir, chromedriver starting Chromium without
# the variable. Each variant prints `REACHED` and how many such processes are
# up once the browsers loaded the page, the gate's exit and output, then, 3s
# after the gate's exit, one line per such process left, kills those by pid, and
# ends `LEFT n`. Exit 0 when every variant left none, 1 when one left any, and 2
# when none did but one did not reach the browsers' load or its gate did not end
# within 120s, or ~/.ghc-wasm/env is missing or a variant unknown.
set -u
here=$(cd "$(dirname "$0")" && pwd)
gate=$here/../../ts-src/page-gate.mjs
# shellcheck source=/dev/null
. ~/.ghc-wasm/env || { echo "no ~/.ghc-wasm/env, so no gate ran"; exit 2; }

# The live processes of the run whose site is $1, by pid, zombies left out.
# The comparisons are bash's own, so no helper's argv carries the site's path.
run_pids() {
  local e pid args env stat
  for e in /proc/[0-9]*/environ; do
    pid=${e#/proc/}
    pid=${pid%/environ}
    [ "$pid" = $$ ] && continue
    args=$(tr '\0' ' ' 2>/dev/null < "/proc/$pid/cmdline") || continue
    env=$(tr '\0' '\n' 2>/dev/null < "$e")
    [[ $'\n'$env$'\n' == *$'\n'"ORPHAN_SITE=$1"$'\n'* || $args == *"$1"* ]] ||
      continue
    stat=$(sed 's/.*) //' "/proc/$pid/stat" 2>/dev/null | cut -d' ' -f1)
    [ "$stat" = Z ] || echo "$pid"
  done
}

# One variant: its status is 0, 1 or 2 as the script's.
variant() {
  local site want=1 vars gate_pid loads signal="" i status pid left=0 pids=()
  case $1 in
    firefox-blocked) vars=(BROWSER="$here/firefox-dead-bidi.sh") ;;
    chromium-blocked) vars=(CHROMEDRIVER="$here/chromedriver-mute.sh") ;;
    sigint) vars=(); want=2; signal=INT ;;
    sigterm) vars=(); want=2; signal=TERM ;;
    *) echo "unknown variant $1"; return 2 ;;
  esac
  echo "=== $1"
  site=$(mktemp -d "${TMPDIR:-/tmp}/page-gate-orphans-XXXXXX")
  cp "$here/serve.mjs" "$site/"
  touch "$site/navigated"
  env PAGE_GATE_BROWSERS=firefox,chromium "${vars[@]}" ORPHAN_SITE="$site" \
    node "$gate" "$site" > "$site/gate.log" 2>&1 &
  gate_pid=$!
  for ((i = 0; i < 600; i++)); do
    loads=$(wc -l < "$site/navigated")
    [ "$loads" -ge $want ] && break
    kill -0 $gate_pid 2>/dev/null || break
    sleep 0.1
  done
  loads=$(wc -l < "$site/navigated")
  if [ "$loads" -ge $want ]; then
    echo "REACHED, $(run_pids "$site" | wc -l) processes of the run up"
  else
    echo "NOT REACHED: $loads of $want loads"
  fi
  if [ -n "$signal" ]; then
    sleep 1
    kill -s $signal $gate_pid
  fi
  for ((i = 0; i < 1200; i++)); do
    kill -0 $gate_pid 2>/dev/null || break
    sleep 0.1
  done
  if kill -0 $gate_pid 2>/dev/null; then
    echo "the gate still running after 120s"
    kill -s KILL $gate_pid
    loads=0
  fi
  wait $gate_pid
  echo "gate exit $?"
  cat "$site/gate.log"
  # Whatever was going to die of the gate's exit has had time to.
  sleep 3
  for pid in $(run_pids "$site"); do
    echo "left: $pid $(tr '\0' ' ' 2>/dev/null < "/proc/$pid/cmdline" |
                        cut -c1-150)"
    pids+=("$pid")
    left=$((left + 1))
  done
  if [ $left -gt 0 ]; then
    kill "${pids[@]}" 2>/dev/null
    sleep 2
    kill -s KILL "${pids[@]}" 2>/dev/null
  fi
  echo "LEFT $left"
  rm -rf "$site"
  if [ $left -gt 0 ]; then
    status=1
  elif [ "$loads" -lt $want ]; then
    status=2
  else
    status=0
  fi
  return $status
}

[ $# -gt 0 ] || set -- firefox-blocked chromium-blocked sigint sigterm
worst=0
for v in "$@"; do
  variant "$v"
  s=$?
  if [ $s -eq 1 ] || { [ $s -eq 2 ] && [ $worst -eq 0 ]; }; then
    worst=$s
  fi
done
echo "exit $worst"
exit $worst
