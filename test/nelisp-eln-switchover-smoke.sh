#!/bin/sh
# S7.7 end-to-end: the old `.neln' path replaced within proven coverage
# (Doc 208).  Two NeLisp-binary processes run concurrently:
#
# - test/nelisp-eln-switchover-driver.el, on a real `.neln' cache compiled
#   here by host Emacs: migrate the cache to genuine GNU-format `.eln'
#   artifacts, route one batch (corrupted artifacts in the middle, plus a
#   pinned genuine GNU artifact), call, unload (previous definitions
#   restored, owners/handles/private mappings released), reload;
# - test/nelisp-eln-switchover-lifecycle-driver.el: defer a release while a
#   routed function is still referenced, then propagate a registry
#   quarantine as recorded fallbacks.
#
# Exit 0 only when both drivers print every expected S77_* check and their
# PASS line, with empty stderr.
set -eu

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repo=${NELISP_ROOT:-$(CDPATH= cd -- "$script_dir/.." && pwd)}
binary=${NELISP_BIN:-$repo/target/nelisp}
emacs_bin=${EMACS_BIN:-emacs}
gnu_identity=${NELISP_ELN_GNU_IDENTITY:-${XDG_CACHE_HOME:-$HOME/.cache}/tmp/eln-gnu-identity-investigation/gnu-identity.eln}
cache_root=${XDG_CACHE_HOME:-$HOME/.cache}
mkdir -p "$cache_root/tmp"
out_dir=$(mktemp -d "$cache_root/tmp/nelisp-eln-switchover.XXXXXX")
keep=${NELISP_ELN_KEEP_ARTIFACTS:-0}

cleanup() {
  status=$?
  if [ "$status" -eq 0 ] && [ "$keep" != 1 ]; then
    rm -rf "$out_dir"
  else
    echo "ARTIFACT_DIR=$out_dir" >&2
  fi
}
trap cleanup EXIT HUP INT TERM

if [ ! -x "$binary" ]; then
  echo "NELISP_BIN is not executable: $binary" >&2
  exit 2
fi
if [ ! -r "$gnu_identity" ]; then
  echo "missing pinned GNU identity artifact: $gnu_identity" >&2
  exit 2
fi

run_driver() {
  # $1 = label, $2 = driver file
  NELISP_S77_NELN=$src.neln NELISP_S77_ELN_DIR=$out_dir/eln \
    NELISP_S77_GNU_IDENTITY=$gnu_identity NELISP_S77_TEST_DIR=$script_dir \
    "$binary" -L "$repo/lisp" -L "$repo/packages/nl-ffi/src" \
    --load "$script_dir/$2" \
    >"$out_dir/$1.stdout" 2>"$out_dir/$1.stderr"
}

check_driver() {
  # $1 = label, $2 = rc, $3 = PASS line prefix, remaining = check names
  label=$1; rc=$2; pass=$3; shift 3
  missing=
  for check in "$@"; do
    grep -Fx "S77_$check=PASS" "$out_dir/$label.stdout" >/dev/null || \
      missing="$missing $check"
  done
  if [ "$rc" -ne 0 ] || [ -n "$missing" ] || [ -s "$out_dir/$label.stderr" ] || \
     ! grep -q "^$pass " "$out_dir/$label.stdout"; then
    cat "$out_dir/$label.stdout"
    head -c 4000 "$out_dir/$label.stderr" >&2
    echo "$label driver failed (rc=$rc) missing checks:$missing" >&2
    return 1
  fi
  grep "^$pass " "$out_dir/$label.stdout"
}

src=$out_dir/s77-fixture.el
run_driver lifecycle nelisp-eln-switchover-lifecycle-driver.el &
lifecycle_pid=$!
cat >"$src" <<'EOF'
;;; s77-fixture.el --- S7.7 switchover fixture -*- lexical-binding: t; -*-
(defun nelisp-s77-const () 23)
(defun nelisp-s77-bad-trunc () 31)
(defun nelisp-s77-ident (x) x)
(defun nelisp-s77-bad-abi () 37)
(defun nelisp-s77-stale () 41)
(defun nelisp-s77-choose (x) (if x 7 0))
(defun nelisp-s77-add1 (x) (+ x 1))
EOF

# The existing `.neln' cache, produced by the ordinary artifact compiler.
pkg_dirs=
for d in "$repo"/packages/*/src; do pkg_dirs="$pkg_dirs -L $d"; done
# shellcheck disable=SC2086
if ! "$emacs_bin" --batch -Q -L "$repo/lisp" -L "$repo/src" $pkg_dirs \
    --eval "(progn (require 'nelisp-artifact) (nelisp-artifact-compile-file \"$src\" \"$src.neln\" nil nil nil nil nil 'neln))" \
    >"$out_dir/neln.stdout" 2>"$out_dir/neln.stderr" || [ ! -r "$src.neln" ]; then
  cat "$out_dir/neln.stderr" >&2
  echo "could not compile the .neln cache" >&2
  wait "$lifecycle_pid" || :
  exit 1
fi
if ! grep -q ':native (:native-section-version' "$src.neln.manifest.el"; then
  echo "the .neln cache has no native section" >&2
  wait "$lifecycle_pid" || :
  exit 1
fi

main_rc=0
run_driver main nelisp-eln-switchover-driver.el || main_rc=$?
lifecycle_rc=0
wait "$lifecycle_pid" || lifecycle_rc=$?

status=0
check_driver main "$main_rc" NELISP-ELN-SWITCHOVER-SMOKE-PASS \
  OLD_PATH MIGRATE CORRUPT BATCH_ROUTES BATCH_CONTINUED BATCH_RESULTS \
  BATCH_NATIVE CALL_FALLBACK_RECORDED BATCH_REGISTRY NEG_RESTORED_CHECKER \
  UNLOAD_RETRACT UNLOAD_RESTORED UNLOAD_RELEASED RELOAD || status=1
check_driver lifecycle "$lifecycle_rc" NELISP-ELN-SWITCHOVER-LIFECYCLE-PASS \
  LIFECYCLE_ROUTED DEFERRED_WHILE_REFERENCED DEFERRED_RELEASED \
  QUARANTINE || status=1
exit "$status"
