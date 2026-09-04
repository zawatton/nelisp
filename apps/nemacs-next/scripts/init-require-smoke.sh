#!/usr/bin/env sh
set -eu

# nemacs init loader reconcile, Phase 2 (session/library-tier wrapped-init
# transport consume regression fixture).  Style follows `require-smoke.sh'
# / `init-smoke.sh': bootstrap REPL heredoc + grep assert, `timeout 90s',
# and a memory cap so a runaway standalone session fails fast instead of
# hanging the batch runner.
#
# Both sessions below share one dynamically generated fixture directory
# (early-init.el does `add-to-list load-path' to a sibling package
# directory; init.el `require's the macro-free `demo-pkg' there).  The
# fixture is generated at run time (rather than checked in statically
# like `require-smoke''s) because `nemacs-wrap-init' only resolves
# `require' against literal string `add-to-list load-path' arguments, so
# the package directory's absolute path has to be baked into
# early-init.el.
#
# `demo-pkg-fn' is deliberately NOT called from within init.el itself:
# the M19-2 wrapped-transport lane pre-notes every resolved `require'd
# package file so the main wrapper's raw per-form `load' of it is
# skipped (the image evaluator cannot reliably nest package loads), and
# only loads the real (lowered) definitions from the `-pkgs-lowered'
# companion *after* the whole wrapper finishes -- exactly mirroring
# `nemacs-gui-file-bridge-runtime-test/standalone-pkg-transpile-in-image',
# which likewise only calls the required package's functions from a
# later, separate evaluation, never synchronously inside init.el.  This
# smoke test follows that same supported shape: it checks `fboundp'
# after `nemacs-init' completes and calls `demo-pkg-fn' itself from the
# harness, not from inside init.el.
#
#   session A (no transport) -- proves the Phase 1 raw-load lane is
#   unaffected: `nemacs-load-user-init-files' falls back to loading
#   early-init.el/init.el directly when no wrapped transport is present
#   at `nemacs--init-wrapped-transport-path'.
#
#   session B (wrapped transport) -- a real host Emacs runs
#   `nemacs-wrap-init' (scripts/nemacs-wrap-init.el) over the same
#   early-init.el/init.el ahead of time, writing the wrapped transport
#   to `nemacs-init-wrapped' under the fixture directory.
#   `nemacs-load-user-init-files' must then consume it through
#   `nemacs-init-transport-consume' (src/nemacs-init-transport.el)
#   instead of raw-loading the two files, and `demo-pkg-fn' must be
#   `fboundp' either way (Doc reconcile plan §5 "init-require-smoke").

ROOT=${NEMACS_NEXT_ROOT:-$(CDPATH= cd -- "$(dirname -- "$0")/../../.." && pwd -P)}
NELISP_BIN=${NELISP_BIN:-${NELISP:-}}
BOOTSTRAP_REPL=${NEMACS_BOOTSTRAP_REPL:-$ROOT/build/nemacs-bootstrap.repl}
EMACS=${EMACS:-emacs}

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
  echo "init-require-smoke: nelisp binary is not executable: $NELISP_BIN" >&2
  exit 1
fi

if [ ! -r "$BOOTSTRAP_REPL" ]; then
  echo "init-require-smoke: bootstrap REPL is not readable: $BOOTSTRAP_REPL" >&2
  echo "init-require-smoke: run make build-nelisp-bootstrap first" >&2
  exit 1
fi

FIXTURE_DIR=$(mktemp -d "${TMPDIR:-/tmp}/init-require-smoke.XXXXXX")
PKG_DIR=$FIXTURE_DIR/pkg
mkdir -p "$PKG_DIR"

tmp=${TMPDIR:-/tmp}/init-require-smoke.$$.repl
out=${TMPDIR:-/tmp}/init-require-smoke.$$.out
err=${TMPDIR:-/tmp}/init-require-smoke.$$.err
tmp_b=${TMPDIR:-/tmp}/init-require-smoke.$$.b.repl
out_b=${TMPDIR:-/tmp}/init-require-smoke.$$.b.out
err_b=${TMPDIR:-/tmp}/init-require-smoke.$$.b.err
wrap_log=${TMPDIR:-/tmp}/init-require-smoke.$$.wrap.out
rm -f "$tmp" "$out" "$err" "$tmp_b" "$out_b" "$err_b" "$wrap_log"
trap 'rm -f "$tmp" "$out" "$err" "$tmp_b" "$out_b" "$err_b" "$wrap_log"; rm -rf "$FIXTURE_DIR"' EXIT

cat > "$PKG_DIR/demo-pkg.el" <<'EOF'
;;; demo-pkg.el --- macro-free fixture package for init-require-smoke  -*- lexical-binding: t; -*-

;;; Commentary:

;; Fixture consumed by `apps/nemacs-next/scripts/init-require-smoke.sh'
;; (nemacs init loader reconcile, Phase 2).  Deliberately avoids
;; `defmacro', `cl-lib', and `define-inline' so it is resolvable both
;; raw-loaded (Phase 1 lane, no transport) and through the wrapped
;; transport (Phase 2 lane, host-side `nemacs-wrap-init' lowering).

;;; Code:

(defvar demo-pkg-loaded t
  "Non-nil once `demo-pkg' has been loaded.")

(defun demo-pkg-fn ()
  "Return a fixed marker string proving `demo-pkg' functions are callable.
Wraps the literal in `identity': the standalone NeLisp runtime
mis-parses a `defun' whose entire body is a single string literal as
an (elided) docstring and returns nil instead (pre-existing runtime
quirk, tracked separately -- same workaround as
`apps/nemacs-next/fixtures/require-smoke/demo-pkg.el')."
  (identity "demo-pkg-fn-ok"))

(provide 'demo-pkg)

;;; demo-pkg.el ends here
EOF

cat > "$FIXTURE_DIR/early-init.el" <<EOF
(add-to-list 'load-path "$PKG_DIR")
EOF

cat > "$FIXTURE_DIR/init.el" <<'EOF'
(require 'demo-pkg)
EOF

# --- session A: no wrapped transport -> Phase 1 raw-load lane --------

cat "$BOOTSTRAP_REPL" > "$tmp"
cat >> "$tmp" <<EOF
(setq load-path (list "$ROOT/src"))
(load "$ROOT/src/emacs-special-buffers.el")
(load "$ROOT/src/nemacs-loadup.el")
(nelisp--write-stdout-bytes (concat "a-pre-fboundp-demo-pkg-fn=" (if (fboundp 'demo-pkg-fn) "yes" "no") "\n"))
(unless nemacs-initialized (nemacs-init))
(nelisp--write-stdout-bytes (concat "a-wrapper-exists=" (if (file-exists-p (nemacs--init-wrapped-transport-path)) "yes" "no") "\n"))
(nelisp--write-stdout-bytes (concat "a-post-fboundp-demo-pkg-fn=" (if (fboundp 'demo-pkg-fn) "yes" "no") "\n"))
(nelisp--write-stdout-bytes (concat "a-featurep-demo-pkg=" (if (featurep 'demo-pkg) "yes" "no") "\n"))
(nelisp--write-stdout-bytes (concat "a-greeting=" (if (fboundp 'demo-pkg-fn) (demo-pkg-fn) "unbound") "\n"))
(nelisp--write-stdout-bytes (concat "a-init-file-had-error=" (if (and (boundp 'init-file-had-error) init-file-had-error) "yes" "no") "\n"))
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
  echo "init-require-smoke: session A (no transport) fail rc=$rc" >&2
  exit 1
fi

for expected in \
  'a-pre-fboundp-demo-pkg-fn=no' \
  'a-wrapper-exists=no' \
  'a-post-fboundp-demo-pkg-fn=yes' \
  'a-featurep-demo-pkg=yes' \
  'a-greeting=demo-pkg-fn-ok' \
  'a-init-file-had-error=no'; do
  if ! grep -q "^$expected$" "$out"; then
    cat "$out" >&2
    cat "$err" >&2
    echo "init-require-smoke: session A (no transport) missing $expected" >&2
    exit 1
  fi
done

# --- host Emacs: generate the wrapped transport ahead of time ---------
#
# Wiring this generator call into a real launcher is Phase 3
# ("生成器 production 配線"); here it stands in for that future launcher
# step so session B below can exercise the Phase 2 consumer against a
# real `nemacs-wrap-init' artifact.

"$EMACS" -Q --batch \
  -l "$ROOT/scripts/nemacs-wrap-init.el" \
  --eval "(princ (format \"wrapped-forms=%d\n\" (nemacs-wrap-init \"$FIXTURE_DIR/nemacs-init-wrapped\" \"$FIXTURE_DIR/early-init.el\" \"$FIXTURE_DIR/init.el\")))" \
  > "$wrap_log" 2>&1
if ! grep -q '^wrapped-forms=2$' "$wrap_log"; then
  cat "$wrap_log" >&2
  echo "init-require-smoke: nemacs-wrap-init did not wrap the expected 2 forms" >&2
  exit 1
fi

if [ ! -r "$FIXTURE_DIR/nemacs-init-wrapped" ]; then
  echo "init-require-smoke: nemacs-wrap-init did not write the wrapper" >&2
  exit 1
fi
if [ ! -r "$FIXTURE_DIR/nemacs-init-wrapped-pkgs-lowered" ]; then
  echo "init-require-smoke: nemacs-wrap-init did not write -pkgs-lowered" >&2
  exit 1
fi

# --- session B: wrapped transport present -> Phase 2 consume lane -----

cat "$BOOTSTRAP_REPL" > "$tmp_b"
cat >> "$tmp_b" <<EOF
(setq load-path (list "$ROOT/src"))
(load "$ROOT/src/emacs-special-buffers.el")
(load "$ROOT/src/nemacs-loadup.el")
(nelisp--write-stdout-bytes (concat "b-pre-fboundp-demo-pkg-fn=" (if (fboundp 'demo-pkg-fn) "yes" "no") "\n"))
(unless nemacs-initialized (nemacs-init))
(nelisp--write-stdout-bytes (concat "b-wrapper-exists=" (if (file-exists-p (nemacs--init-wrapped-transport-path)) "yes" "no") "\n"))
(nelisp--write-stdout-bytes (concat "b-post-fboundp-demo-pkg-fn=" (if (fboundp 'demo-pkg-fn) "yes" "no") "\n"))
(nelisp--write-stdout-bytes (concat "b-featurep-demo-pkg=" (if (featurep 'demo-pkg) "yes" "no") "\n"))
(nelisp--write-stdout-bytes (concat "b-greeting=" (if (fboundp 'demo-pkg-fn) (demo-pkg-fn) "unbound") "\n"))
(nelisp--write-stdout-bytes (concat "b-init-file-had-error=" (if (and (boundp 'init-file-had-error) init-file-had-error) "yes" "no") "\n"))
(nelisp--write-stdout-bytes (concat "b-applied=" (number-to-string nemacs-init--applied) "\n"))
(nelisp--write-stdout-bytes (concat "b-report-exists=" (if (file-exists-p (concat (nemacs--init-wrapped-transport-path) "-report")) "yes" "no") "\n"))
,quit
EOF

set +e
( ulimit -v 4194304 2>/dev/null
  NEMACS_USER_EMACS_DIRECTORY=$FIXTURE_DIR \
    timeout 90s "$NELISP_BIN" --repl --no-prompt --no-print \
    < "$tmp_b" > "$out_b" 2> "$err_b" )
rc_b=$?
set -e

if [ "$rc_b" -ne 0 ]; then
  cat "$out_b" >&2
  cat "$err_b" >&2
  echo "init-require-smoke: session B (wrapped transport) fail rc=$rc_b" >&2
  exit 1
fi

for expected in \
  'b-pre-fboundp-demo-pkg-fn=no' \
  'b-wrapper-exists=yes' \
  'b-post-fboundp-demo-pkg-fn=yes' \
  'b-featurep-demo-pkg=yes' \
  'b-greeting=demo-pkg-fn-ok' \
  'b-init-file-had-error=no' \
  'b-applied=2' \
  'b-report-exists=yes'; do
  if ! grep -q "^$expected$" "$out_b"; then
    cat "$out_b" >&2
    cat "$err_b" >&2
    echo "init-require-smoke: session B (wrapped transport) missing $expected" >&2
    exit 1
  fi
done

echo "init-require-smoke: a-post-fboundp-demo-pkg-fn=yes (no transport, Phase 1 raw load)"
echo "init-require-smoke: b-post-fboundp-demo-pkg-fn=yes (wrapped transport, Phase 2 consume)"
echo "init-require-smoke: b-applied=2"
echo "init-require-smoke: ok"
