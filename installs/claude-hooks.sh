#!/bin/bash -x

# Merge the claude-notify-hook.sh hooks into Claude Code's user settings.
# Idempotent: replaces only hook entries that run claude-notify-hook.sh and
# leaves every other (site-local) setting and hook untouched.
# Usage: claude-hooks.sh [SETTINGS_JSON]   (default: ~/.claude/settings.json)

set -euo pipefail

SETTINGS=${1:-${HOME}/.claude/settings.json}
HOOK=$(realpath "$(dirname "$0")/../bin/claude-notify-hook.sh")

mkdir -p "$(dirname "${SETTINGS}")"
test -e "${SETTINGS}" || echo '{}' > "${SETTINGS}"

TMP=$(mktemp "${SETTINGS}.XXXXXX")
jq --arg hook "${HOOK}" '
  def ours: .hooks | any(.command | test("claude-notify-hook\\.sh"));
  def entry($arg): {hooks: [{type: "command",
                             command: "\($hook) \($arg) 2>/dev/null || true"}]};
  .hooks //= {}
  | reduce (["UserPromptSubmit", "start"], ["Notification", "waiting"], ["Stop", "done"]) as [$event, $arg]
      (.; .hooks[$event] = ([.hooks[$event][]? | select(ours | not)] + [entry($arg)]))
' "${SETTINGS}" > "${TMP}"
mv "${TMP}" "${SETTINGS}"
