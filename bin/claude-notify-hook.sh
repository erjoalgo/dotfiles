#!/usr/bin/env bash
# Claude Code notification hook. Usage: claude-notify-hook.sh <start|waiting|done>
# Reads the hook JSON payload on stdin. Only notifies when the current turn
# has been running for at least CLAUDE_NOTIFY_MIN_SECONDS (default 60).
set -u
event="${1:-done}"
min="${CLAUDE_NOTIFY_MIN_SECONDS:-60}"
payload=$(cat)

session=$(jq -r '.session_id // "default"' <<<"$payload")
project=$(basename "$(jq -r '.cwd // empty' <<<"$payload")")
stamp="${XDG_RUNTIME_DIR:-/tmp}/claude-turn-start-$session"
now=$(date +%s)

if [ "$event" = start ]; then
  echo "$now" > "$stamp"
  exit 0
fi

start=$(cat "$stamp" 2>/dev/null || echo "$now")
elapsed=$((now - start))
[ "$elapsed" -ge "$min" ] || exit 0
took="$((elapsed / 60))m$((elapsed % 60))s"

case "$event" in
  waiting)
    type=$(jq -r '.notification_type // empty' <<<"$payload")
    case "$type" in idle_prompt|auth_success) exit 0 ;; esac
    msg=$(jq -r '.message // "Claude needs your input"' <<<"$payload")
    notify-send-stumpwm -m "Claude waiting [$project, $took]: $msg"
    ;;
  done)
    notify-send-stumpwm -m "Claude responded [$project, $took]"
    ;;
esac
