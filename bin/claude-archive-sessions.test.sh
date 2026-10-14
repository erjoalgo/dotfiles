#!/bin/bash
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
t=$(mktemp -d)
export CLAUDE_CONFIG_DIR="$t/claude"
p="$CLAUDE_CONFIG_DIR/projects/-work-repo"
mkdir -p "$p/bbb" "$t/work/repo"
printf '%s\n' '{"type":"user","message":{"content":"first prompt aaa"}}' > "$p/aaa.jsonl"
printf '%s\n' '{"type":"user","message":{"content":"ignored"}}' '{"type":"ai-title","aiTitle":"AI title bbb"}' > "$p/bbb.jsonl"
printf '%s\n' '{"type":"ai-title","aiTitle":"x"}' '{"type":"custom-title","customTitle":"Custom ccc"}' > "$p/ccc.jsonl"
touch -d '3 days ago' "$p/aaa.jsonl"; touch -d '2 days ago' "$p/bbb.jsonl"; touch -d '1 day ago' "$p/ccc.jsonl"
out=$(printf '9\n2-3\nq\n' | "$here/claude-archive-sessions" --project /work/repo)
echo "$out"
grep -q 'Custom ccc' <<<"$out"; grep -q 'AI title bbb' <<<"$out"; grep -q 'first prompt aaa' <<<"$out"
grep -q 'Invalid selection: 9 is out of range' <<<"$out"
a="$CLAUDE_CONFIG_DIR/archived-sessions/-work-repo"
test -f "$a/bbb.jsonl" && test -d "$a/bbb" && test -f "$a/aaa.jsonl"
test -f "$p/ccc.jsonl" && test ! -e "$p/bbb.jsonl" && test ! -e "$p/aaa.jsonl"
rm -r -- "$t"
echo PASS
