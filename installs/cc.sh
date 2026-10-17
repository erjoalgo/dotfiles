#!/bin/bash -x

set -euo pipefail

pnpm add -g @anthropic-ai/claude-code
npm install -g pnpm@latest
claude --version
