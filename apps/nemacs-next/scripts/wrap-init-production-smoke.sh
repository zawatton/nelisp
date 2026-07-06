#!/usr/bin/env sh
set -eu

# nemacs init loader reconcile, Phase 3 (production wrap-init generator
# wiring regression fixture).  Style follows `init-require-smoke.sh': a
# dynamically generated fixture directory (early-init.el does `add-to-list
# load-path' to a sibling package directory; init.el `require's the
# macro-free `demo-pkg' there), `timeout 90s', and a memory cap.
#
# Unlike `init-require-smoke.sh' -- which stands in for the not-yet-wired
# launcher step by calling `nemacs-wrap-init' itself before the session
# assertions -- this smoke drives the REAL production launcher, `bin/nemacs'
# (nelisp driver), end to end and asserts on its side effects:
#
#   A. default (no -Q, host Emacs present): `bin/nemacs' generates the
#      wrapped transport itself under `NEMACS_USER_EMACS_DIRECTORY' ahead of
#      dispatch, and the running process picks it up (demo-pkg-fn becomes
#      fboundp) -- proving the generator is wired into the launcher, not
#      just the underlying consumer machinery (already covered by Phase 2's
#      `init-require-smoke.sh').
#
#   B. -Q: the launcher must not generate a transport at all (and the
#      loadup init gate skips loading either way) -- the double closure
#      from the reconcile plan's Phase 3 risk list.
#
#   C. host Emacs unavailable (`NEMACS_EMACS' pointed at a nonexistent
#      binary): the launcher must silently degrade to the Phase 1 raw-load
#      lane instead of failing the whole process.

ROOT=${NEMACS_NEXT_ROOT:-$(CDPATH= cd -- "$(dirname -- "$0")/../../.." && pwd -P)}
NELISP_BIN=${NELISP_BIN:-${NELISP:-}}
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
  echo "wrap-init-production-smoke: nelisp binary is not executable: $NELISP_BIN" >&2
  exit 1
fi

if ! command -v "$EMACS" >/dev/null 2>&1; then
  echo "wrap-init-production-smoke: host Emacs not available: $EMACS" >&2
  exit 1
fi

mk_fixture() {
  # $1 = fixture root to create
  fdir=$1
  mkdir -p "$fdir/pkg"
  cat > "$fdir/pkg/demo-pkg.el" <<'EOF'
;;; demo-pkg.el --- macro-free fixture package for wrap-init-production-smoke  -*- lexical-binding: t; -*-
(defvar demo-pkg-loaded t
  "Non-nil once `demo-pkg' has been loaded.")
(defun demo-pkg-fn ()
  "Return a fixed marker string proving `demo-pkg' functions are callable."
  (identity "demo-pkg-fn-ok"))
(provide 'demo-pkg)
EOF
  cat > "$fdir/early-init.el" <<EOF
(add-to-list 'load-path "$fdir/pkg")
EOF
  cat > "$fdir/init.el" <<'EOF'
(require 'demo-pkg)
EOF
}

FIXTURE_A=$(mktemp -d "${TMPDIR:-/tmp}/wrap-init-production-smoke-a.XXXXXX")
FIXTURE_B=$(mktemp -d "${TMPDIR:-/tmp}/wrap-init-production-smoke-b.XXXXXX")
FIXTURE_C=$(mktemp -d "${TMPDIR:-/tmp}/wrap-init-production-smoke-c.XXXXXX")
marker_a=${TMPDIR:-/tmp}/wrap-init-production-smoke.$$.a.marker
marker_b=${TMPDIR:-/tmp}/wrap-init-production-smoke.$$.b.marker
marker_c=${TMPDIR:-/tmp}/wrap-init-production-smoke.$$.c.marker
log_a=${TMPDIR:-/tmp}/wrap-init-production-smoke.$$.a.log
log_b=${TMPDIR:-/tmp}/wrap-init-production-smoke.$$.b.log
log_c=${TMPDIR:-/tmp}/wrap-init-production-smoke.$$.c.log
rm -f "$marker_a" "$marker_b" "$marker_c" "$log_a" "$log_b" "$log_c"
trap 'rm -rf "$FIXTURE_A" "$FIXTURE_B" "$FIXTURE_C"; rm -f "$marker_a" "$marker_b" "$marker_c" "$log_a" "$log_b" "$log_c"' EXIT

mk_fixture "$FIXTURE_A"
mk_fixture "$FIXTURE_B"
mk_fixture "$FIXTURE_C"

# --- A: default -- launcher generates the transport itself ---------------

set +e
NELISP="$NELISP_BIN" NEMACS_EMACS="$EMACS" \
  NEMACS_USER_EMACS_DIRECTORY="$FIXTURE_A" \
  timeout 90s "$ROOT/bin/nemacs" --driver=nelisp --batch --no-banner \
  --eval "(write-region (if (fboundp (quote demo-pkg-fn)) \"yes\" \"no\") nil \"$marker_a\" nil (quote silent))" \
  > "$log_a" 2>&1
rc_a=$?
set -e

if [ "$rc_a" -ne 0 ]; then
  cat "$log_a" >&2
  echo "wrap-init-production-smoke: session A (default) fail rc=$rc_a" >&2
  exit 1
fi
if [ ! -f "$FIXTURE_A/nemacs-init-wrapped" ]; then
  cat "$log_a" >&2
  echo "wrap-init-production-smoke: session A did not generate nemacs-init-wrapped" >&2
  exit 1
fi
if [ ! -f "$FIXTURE_A/nemacs-init-wrapped-pkgs-lowered" ]; then
  cat "$log_a" >&2
  echo "wrap-init-production-smoke: session A did not generate nemacs-init-wrapped-pkgs-lowered" >&2
  exit 1
fi
if ! grep -q '^yes$' "$marker_a" 2>/dev/null; then
  cat "$log_a" >&2
  echo "wrap-init-production-smoke: session A demo-pkg-fn not fboundp (marker=$(cat "$marker_a" 2>/dev/null || echo MISSING))" >&2
  exit 1
fi

# --- B: -Q -- no transport generated, nothing loaded ----------------------

set +e
NELISP="$NELISP_BIN" NEMACS_EMACS="$EMACS" \
  NEMACS_USER_EMACS_DIRECTORY="$FIXTURE_B" \
  timeout 90s "$ROOT/bin/nemacs" --driver=nelisp -Q --batch --no-banner \
  --eval "(write-region (if (fboundp (quote demo-pkg-fn)) \"yes\" \"no\") nil \"$marker_b\" nil (quote silent))" \
  > "$log_b" 2>&1
rc_b=$?
set -e

if [ "$rc_b" -ne 0 ]; then
  cat "$log_b" >&2
  echo "wrap-init-production-smoke: session B (-Q) fail rc=$rc_b" >&2
  exit 1
fi
if [ -f "$FIXTURE_B/nemacs-init-wrapped" ]; then
  echo "wrap-init-production-smoke: session B (-Q) unexpectedly generated nemacs-init-wrapped" >&2
  exit 1
fi
if ! grep -q '^no$' "$marker_b" 2>/dev/null; then
  cat "$log_b" >&2
  echo "wrap-init-production-smoke: session B (-Q) demo-pkg-fn unexpectedly fboundp" >&2
  exit 1
fi

# --- C: host Emacs unavailable -- silent degrade, no crash -----------------

set +e
NELISP="$NELISP_BIN" NEMACS_EMACS=/nonexistent-emacs-for-wrap-init-production-smoke \
  NEMACS_USER_EMACS_DIRECTORY="$FIXTURE_C" \
  timeout 90s "$ROOT/bin/nemacs" --driver=nelisp --batch --no-banner \
  --eval "(write-region \"reached\" nil \"$marker_c\" nil (quote silent))" \
  > "$log_c" 2>&1
rc_c=$?
set -e

if [ "$rc_c" -ne 0 ]; then
  cat "$log_c" >&2
  echo "wrap-init-production-smoke: session C (host Emacs unavailable) fail rc=$rc_c" >&2
  exit 1
fi
if [ -f "$FIXTURE_C/nemacs-init-wrapped" ]; then
  echo "wrap-init-production-smoke: session C unexpectedly generated nemacs-init-wrapped without host Emacs" >&2
  exit 1
fi
if ! grep -q '^reached$' "$marker_c" 2>/dev/null; then
  cat "$log_c" >&2
  echo "wrap-init-production-smoke: session C did not reach the eval marker" >&2
  exit 1
fi

echo "wrap-init-production-smoke: A wrapper-generated=yes demo-pkg-fn=yes"
echo "wrap-init-production-smoke: B -Q wrapper-generated=no demo-pkg-fn=no"
echo "wrap-init-production-smoke: C host-absent degrade reached=yes wrapper-generated=no"
echo "wrap-init-production-smoke: ok"
