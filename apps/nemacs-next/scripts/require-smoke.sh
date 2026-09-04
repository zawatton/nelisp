#!/usr/bin/env sh
set -eu

# nemacs init loader reconcile, Phase 1 (substrate require/provide loud
# regression fixture).  Style follows `init-smoke.sh': bootstrap REPL
# heredoc + grep assert, `timeout 90s', and a memory cap so a runaway
# standalone session fails fast instead of hanging the batch runner.

ROOT=${NEMACS_NEXT_ROOT:-$(CDPATH= cd -- "$(dirname -- "$0")/../../.." && pwd -P)}
NELISP_BIN=${NELISP_BIN:-${NELISP:-}}
BOOTSTRAP_REPL=${NEMACS_BOOTSTRAP_REPL:-$ROOT/build/nemacs-bootstrap.repl}
FIXTURE_DIR=${NEMACS_USER_EMACS_DIRECTORY:-$ROOT/apps/nemacs-next/fixtures/require-smoke}

if [ -z "$NELISP_BIN" ]; then
  if [ -x "$ROOT/../nelisp/target/nelisp" ]; then
    NELISP_BIN=$ROOT/../nelisp/target/nelisp
  elif [ -x "$ROOT/vendor/nelisp/target/nelisp" ]; then
    NELISP_BIN=$ROOT/vendor/nelisp/target/nelisp
  else
    NELISP_BIN=$ROOT/../nelisp/target/nelisp
  fi
fi

if [ ! -x "$NELISP_BIN" ]; then
  echo "require-smoke: nelisp binary is not executable: $NELISP_BIN" >&2
  exit 1
fi

if [ ! -r "$BOOTSTRAP_REPL" ]; then
  echo "require-smoke: bootstrap REPL is not readable: $BOOTSTRAP_REPL" >&2
  echo "require-smoke: run make build-nelisp-bootstrap first" >&2
  exit 1
fi

tmp=${TMPDIR:-/tmp}/require-smoke.$$.repl
out=${TMPDIR:-/tmp}/require-smoke.$$.out
err=${TMPDIR:-/tmp}/require-smoke.$$.err
tmp_neg=${TMPDIR:-/tmp}/require-smoke.$$.neg.repl
out_neg=${TMPDIR:-/tmp}/require-smoke.$$.neg.out
err_neg=${TMPDIR:-/tmp}/require-smoke.$$.neg.err
rm -f "$tmp" "$out" "$err" "$tmp_neg" "$out_neg" "$err_neg"
trap 'rm -f "$tmp" "$out" "$err" "$tmp_neg" "$out_neg" "$err_neg"' EXIT

# --- session A: positive case through the real Lane A loader --------
#
# `NEMACS_USER_EMACS_DIRECTORY' points at a fixture whose init.el
# requires a plain, macro-free package (`demo-pkg').  This proves the
# loud substrate `require'/`provide' in `emacs-fns.el' resolves through
# `nemacs-init' -> `nemacs-load-user-init-files' -> `nemacs--load-init-file'
# the same way any real consumer init.el would (Doc 35 Lane A).

cat "$BOOTSTRAP_REPL" > "$tmp"
cat >> "$tmp" <<EOF
(setq load-path (list "$ROOT/src" "$ROOT/apps/nemacs-next/lisp" "$ROOT/apps/nemacs-next/fixtures/require-smoke"))
(load "$ROOT/src/emacs-special-buffers.el")
(load "$ROOT/src/nemacs-loadup.el")
(nelisp--write-stdout-bytes (concat "pre-fboundp-require=" (if (fboundp 'require) "yes" "no") "\n"))
(nelisp--write-stdout-bytes (concat "pre-fboundp-demo-pkg-greeting=" (if (fboundp 'require-smoke-demo-pkg-greeting) "yes" "no") "\n"))
(unless nemacs-initialized (nemacs-init))
(nelisp--write-stdout-bytes (concat "post-fboundp-demo-pkg-greeting=" (if (fboundp 'require-smoke-demo-pkg-greeting) "yes" "no") "\n"))
(nelisp--write-stdout-bytes (concat "featurep-demo-pkg=" (if (featurep 'demo-pkg) "yes" "no") "\n"))
(nelisp--write-stdout-bytes (concat "init-greeting=" (if (boundp 'require-smoke-init-greeting) require-smoke-init-greeting "unbound") "\n"))
(nelisp--write-stdout-bytes (concat "init-file-had-error=" (if (and (boundp 'init-file-had-error) init-file-had-error) "yes" "no") "\n"))
,quit
EOF

set +e
( ulimit -v 4194304 2>/dev/null
  NEMACS_USER_EMACS_DIRECTORY=$FIXTURE_DIR \
    timeout 90s "$NELISP_BIN" --repl --no-prompt --no-print \
    < "$tmp" > "$out" 2> "$err" )
rc=$?
set -e

if [ "$rc" -ne 0 ]; then
  cat "$out" >&2
  cat "$err" >&2
  echo "require-smoke: session A (loader) fail rc=$rc" >&2
  exit 1
fi

for expected in \
  'pre-fboundp-require=yes' \
  'pre-fboundp-demo-pkg-greeting=no' \
  'post-fboundp-demo-pkg-greeting=yes' \
  'featurep-demo-pkg=yes' \
  'init-greeting=demo-pkg-hello' \
  'init-file-had-error=no'; do
  if ! grep -q "^$expected$" "$out"; then
    cat "$out" >&2
    cat "$err" >&2
    echo "require-smoke: session A (loader) missing $expected" >&2
    exit 1
  fi
done

# --- session B: negative cases on the raw substrate ------------------
#
# Exercises `emacs-fns.el' `require' directly, with no loader involved:
#  - a feature with no resolvable file on `load-path' must be a loud
#    `error' without NOERROR, and nil with NOERROR -- never the
#    historical silent "success" that returned the feature symbol
#    regardless of whether anything was ever loaded or provided.
#  - a real, loadable file that never calls `provide' (`broken-pkg')
#    must be treated the same way: loud without NOERROR, nil with
#    NOERROR, and `featurep' must stay nil in both cases.

cat "$BOOTSTRAP_REPL" > "$tmp_neg"
cat >> "$tmp_neg" <<EOF
(setq load-path (list "$ROOT/apps/nemacs-next/fixtures/require-smoke"))
(nelisp--write-stdout-bytes (concat "missing-noerror-result=" (prin1-to-string (require 'require-smoke-missing-feature nil t)) "\n"))
(nelisp--write-stdout-bytes (concat "missing-noerror-fboundp=" (if (fboundp 'require-smoke-missing-feature) "yes" "no") "\n"))
(require 'require-smoke-missing-feature)
(nelisp--write-stdout-bytes (concat "missing-loud-continued=" "reached" "\n"))
(nelisp--write-stdout-bytes (concat "broken-noerror-result=" (prin1-to-string (require 'broken-pkg nil t)) "\n"))
(nelisp--write-stdout-bytes (concat "broken-noerror-featurep=" (if (featurep 'broken-pkg) "yes" "no") "\n"))
(require 'broken-pkg)
(nelisp--write-stdout-bytes (concat "broken-loud-continued=" "reached" "\n"))
(nelisp--write-stdout-bytes (concat "broken-loud-featurep=" (if (featurep 'broken-pkg) "yes" "no") "\n"))
(nelisp--write-stdout-bytes (concat "broken-loud-file-executed=" (if (and (boundp 'require-smoke-broken-pkg-loaded) require-smoke-broken-pkg-loaded) "yes" "no") "\n"))
,quit
EOF

set +e
( ulimit -v 4194304 2>/dev/null
  timeout 90s "$NELISP_BIN" --repl --no-prompt --no-print \
    < "$tmp_neg" > "$out_neg" 2> "$err_neg" )
rc_neg=$?
set -e

if [ "$rc_neg" -ne 0 ]; then
  cat "$out_neg" >&2
  cat "$err_neg" >&2
  echo "require-smoke: session B (negative) fail rc=$rc_neg" >&2
  exit 1
fi

for expected in \
  'missing-noerror-result=nil' \
  'missing-noerror-fboundp=no' \
  'missing-loud-continued=reached' \
  'broken-noerror-result=nil' \
  'broken-noerror-featurep=no' \
  'broken-loud-continued=reached' \
  'broken-loud-featurep=no' \
  'broken-loud-file-executed=yes'; do
  if ! grep -q "^$expected$" "$out_neg"; then
    cat "$out_neg" >&2
    cat "$err_neg" >&2
    echo "require-smoke: session B (negative) missing $expected" >&2
    exit 1
  fi
done

# The loud (non-NOERROR) paths above must actually have signalled --
# confirm the standalone runtime's uncaught-error channel saw both
# feature names, so "missing-loud-continued"/"broken-loud-continued"
# are proof of loud-then-recovered, not proof nothing happened.
for expected_marker in \
  'require-smoke-missing-feature' \
  'broken-pkg'; do
  if ! grep -q "$expected_marker" "$err_neg"; then
    cat "$out_neg" >&2
    cat "$err_neg" >&2
    echo "require-smoke: session B (negative) missing loud stderr marker for $expected_marker" >&2
    exit 1
  fi
done

echo "require-smoke: pre-fboundp-require=yes"
echo "require-smoke: post-fboundp-demo-pkg-greeting=yes"
echo "require-smoke: featurep-demo-pkg=yes"
echo "require-smoke: init-file-had-error=no"
echo "require-smoke: missing-noerror-result=nil"
echo "require-smoke: missing-feature loud error confirmed"
echo "require-smoke: broken-noerror-result=nil"
echo "require-smoke: broken-pkg loud error confirmed"
echo "require-smoke: ok"
