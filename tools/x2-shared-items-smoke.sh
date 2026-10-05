#!/usr/bin/env bash
# GNU comparison units use the runner's documented probe format / --unit API.
set -euo pipefail
cd "$(dirname "$0")/.."
bash test/nelisp-emacs-lib/c-core-parity-smoke.sh run --unit x2-default,x2-reader,x2-marker
python3 tools/x2-reader-literals-smoke.py
