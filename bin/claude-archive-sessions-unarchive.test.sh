#!/bin/bash
set -euo pipefail
bin="$(cd "$(dirname "$0")" && pwd)/claude-archive-sessions"
t=$(mktemp -d)
export CLAUDE_CONFIG_DIR="$t/claude"
p="$CLAUDE_CONFIG_DIR/projects/-work-repo"; a="$CLAUDE_CONFIG_DIR/archived-sessions/-work-repo"
mkdir -p "$p/bbb"
printf '%s\n' '{"type":"user","message":{"content":"prompt aaa"}}' > "$p/aaa.jsonl"
printf '%s\n' '{"type":"user","message":{"content":"prompt bbb"}}' > "$p/bbb.jsonl"
touch -d '2 days ago' "$p/aaa.jsonl"
printf '1-2\nq\n' | "$bin" --project /work/repo >/dev/null
test -f "$a/aaa.jsonl" && test -f "$a/bbb.jsonl" && test -d "$a/bbb"
ln -s nobody@host.1:1 "$a/.#bbb.jsonl"
out=$(printf '1\nq\n' | "$bin" -u --project /work/repo)
echo "$out"
grep -q 'Unarchive which?' <<<"$out"; grep -q 'unarchived 1: prompt bbb' <<<"$out"
test -f "$p/bbb.jsonl" && test -d "$p/bbb" && test ! -e "$a/bbb.jsonl" && test -f "$a/aaa.jsonl"
out=$(printf 'q\n' | "$bin" --unarchive --all); grep -q 'prompt aaa' <<<"$out"
echo PASS
