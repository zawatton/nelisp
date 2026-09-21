#!/usr/bin/env bash
set -euo pipefail

tool_dir=$(cd "$(dirname "$0")" && pwd)
repo_dir=$(cd "$tool_dir/.." && pwd)

cd "$repo_dir"
exec emacs --batch -Q -L tools \
  -l nelisp-bytecode-opcode-parity \
  -f nelisp-bytecode-opcode-parity-run
