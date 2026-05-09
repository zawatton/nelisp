#!/usr/bin/env bash
# Phase A2 (Doc anvil-runtime pure-elisp roadmap, 2026-05-09)
# Smoke test — emacs-stdio shim on standalone NeLisp.
#
# Validates:
#   1. emacs-stdio.el loads under standalone NeLisp `exec' mode
#   2. read-stdin-bytes-backed line reader splits multi-line input
#   3. chunk-crossing reassembly works (lines > 4096 byte chunk size)
#   4. install-stdin-shim overrides the bulk-stub nil binding
#   5. `read-from-minibuffer' becomes functional after install
#
# Requires: nelisp standalone binary built at
#   $NELISP_BIN  (default: ~/Cowork/Notes/dev/nelisp/target/release/nelisp)
#
# Usage:
#   ./tests/phase-a2-stdio-shim-smoke.sh           # run all checks
#   NELISP_BIN=/abs/path ./phase-a2-stdio-shim-smoke.sh

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
NELISP_BIN="${NELISP_BIN:-$HOME/Cowork/Notes/dev/nelisp/target/release/nelisp}"
STDIO_EL="$REPO_ROOT/src/emacs-stdio.el"
STUB_BULK_EL="$REPO_ROOT/src/emacs-stub-bulk.el"

if [ ! -x "$NELISP_BIN" ]; then
    echo "FAIL: nelisp binary not found at $NELISP_BIN" >&2
    echo "Build it first: (cd ~/Cowork/Notes/dev/nelisp && cargo build --release -p build-tool --bin nelisp)" >&2
    exit 1
fi
if [ ! -f "$STDIO_EL" ]; then
    echo "FAIL: emacs-stdio.el missing at $STDIO_EL" >&2
    exit 1
fi

TMPDIR="$(mktemp -d)"
trap 'rm -rf "$TMPDIR"' EXIT

pass=0
fail=0

run_case() {
    local label="$1"
    local fixture_el="$2"
    local stdin_input="$3"
    local expect_substr="$4"

    actual="$(printf '%s' "$stdin_input" | "$NELISP_BIN" exec "$fixture_el" 2>&1 || true)"
    if printf '%s' "$actual" | grep -q -F "$expect_substr"; then
        echo "PASS: $label"
        pass=$((pass + 1))
    else
        echo "FAIL: $label"
        echo "  expected substring: $expect_substr"
        echo "  actual: $actual"
        fail=$((fail + 1))
    fi
}

#### Case 1: direct read-line on 2 lines ####
cat > "$TMPDIR/case1.el" <<EOF
(progn
  (load "$STDIO_EL")
  (princ (format "L1=%S " (emacs-stdio-read-line)))
  (princ (format "L2=%S " (emacs-stdio-read-line)))
  (princ (format "EOF=%S\n" (emacs-stdio-read-line))))
EOF
run_case "direct-read-2-lines" "$TMPDIR/case1.el" \
    $'alpha\nbeta\n' \
    'L1="alpha" L2="beta" EOF=nil'

#### Case 2: shim install on unbound symbol ####
cat > "$TMPDIR/case2.el" <<EOF
(progn
  (load "$STDIO_EL")
  (princ (format "before=%S " (fboundp (quote read-from-minibuffer))))
  (princ (format "install=%S " (emacs-stdio-install-stdin-shim)))
  (princ (format "after=%S " (fboundp (quote read-from-minibuffer))))
  (princ (format "L1=%S\n" (read-from-minibuffer ""))))
EOF
run_case "install-from-unbound" "$TMPDIR/case2.el" \
    $'gamma\n' \
    'before=nil install=t after=t L1="gamma"'

#### Case 3: shim overrides bulk-stub nil closure ####
cat > "$TMPDIR/case3.el" <<EOF
(progn
  (load "$STUB_BULK_EL")
  (princ (format "stub-call=%S " (read-from-minibuffer "p")))
  (load "$STDIO_EL")
  (princ (format "install=%S " (emacs-stdio-install-stdin-shim)))
  (princ (format "L1=%S\n" (read-from-minibuffer ""))))
EOF
run_case "override-bulk-stub" "$TMPDIR/case3.el" \
    $'delta\n' \
    'stub-call=nil install=t L1="delta"'

#### Case 4: chunk-crossing — line > 4096 bytes ####
big="$(printf 'X%.0s' {1..6000})"
cat > "$TMPDIR/case4.el" <<EOF
(progn
  (load "$STDIO_EL")
  (let ((line (emacs-stdio-read-line)))
    (princ (format "len=%d\n" (length line)))))
EOF
run_case "chunk-cross-6000-bytes" "$TMPDIR/case4.el" \
    "${big}"$'\n' \
    "len=6000"

#### Summary ####
total=$((pass + fail))
echo "----"
echo "Phase A2 stdio smoke: $pass/$total passed"
if [ "$fail" -gt 0 ]; then
    exit 1
fi
exit 0
