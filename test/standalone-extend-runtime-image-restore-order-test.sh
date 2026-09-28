#!/usr/bin/env bash
# Regression test for the D3 defect (docs/design/33-emacs-core-substrate-
# priority-plan.org, section "* 10."): `extend-runtime-image' used to run
# its restore bootstrap with NO native prelude priming (unlike
# `eval-runtime-image'/`exec-runtime-image'), so the embedded restore
# blob's own internal `require' reached `do-after-load-evaluation' before
# that same blob's copy of `subr.el' had (re-)established `string-match-p',
# aborting with `void-function: (string-match-p)' on every
# `extend-runtime-image' call -- even for a trivial 3-line base with no
# magit/library content at all.  Runnable directly against a built
# standalone binary, independent of the wider `standalone-reader-test'
# smoke battery.
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
binary="${NELISP_BIN:-$repo_root/target/nelisp}"
if [[ ! -x "$binary" ]]; then
  echo "standalone-extend-runtime-image-restore-order-test: missing executable: $binary" >&2
  exit 2
fi

tmp_dir="$(mktemp -d)"
trap 'rm -rf "$tmp_dir"' EXIT

base="$tmp_dir/base.nlri"
loadsrc="$tmp_dir/load.el"
out="$tmp_dir/out.nlri"
missing_base="$tmp_dir/does-not-exist.nlri"
missing_out="$tmp_dir/missing-out.nlri"

# Minimal base: no magit, no nelisp-emacs-lib content, matching the repro
# in the design doc (a 3-line dumped base already reproduces the crash).
if ! "$binary" dump-runtime-image "$base" "(setq nel-extend-restore-base 10)" \
     >"$tmp_dir/dump.out" 2>"$tmp_dir/dump.err"; then
  echo "standalone-extend-runtime-image-restore-order-test: dump-runtime-image failed:" >&2
  cat "$tmp_dir/dump.err" >&2
  exit 1
fi

printf '(defun nel-extend-restore-loaded () 5)\n' > "$loadsrc"

# The crash reproduces on the restore bootstrap itself, before this
# command's own FORM ever runs -- so a bare, content-free extend already
# exercises the defect; this call also covers `--load' + inline FORM
# concatenation ordering into OUT-IMAGE.
if ! "$binary" extend-runtime-image "$base" "$out" \
     --load "$loadsrc" "(setq nel-extend-restore-add 27)" \
     >"$tmp_dir/extend.out" 2>"$tmp_dir/extend.err"; then
  echo "standalone-extend-runtime-image-restore-order-test: FAIL -- extend-runtime-image aborted:" >&2
  cat "$tmp_dir/extend.err" >&2
  if grep -q "void-function: (string-match-p)" "$tmp_dir/extend.err"; then
    echo "standalone-extend-runtime-image-restore-order-test: this is the D3 restore-order regression (string-match-p void during do-after-load-evaluation)" >&2
  fi
  if grep -q "void-function: (advice-member-p)" "$tmp_dir/extend.err"; then
    echo "standalone-extend-runtime-image-restore-order-test: the embedded command source's (require 'nelisp-bytecode) loaded nelisp-jit, whose install-on-load needs nadvice (absent on the standalone)" >&2
  fi
  exit 1
fi
if [[ -s "$tmp_dir/extend.err" ]]; then
  echo "standalone-extend-runtime-image-restore-order-test: FAIL -- extend-runtime-image wrote to stderr:" >&2
  cat "$tmp_dir/extend.err" >&2
  exit 1
fi

actual="$("$binary" eval-runtime-image "$out" \
  "(+ nel-extend-restore-base nel-extend-restore-add (nel-extend-restore-loaded))")"
if [[ "$actual" != "42" ]]; then
  echo "standalone-extend-runtime-image-restore-order-test: FAIL -- extended image result mismatch: $actual (expected 42)" >&2
  exit 1
fi

# Many top-level definitions through nested loads (the magit bundle
# shape: one manifest `load'ing part files).  A binding made by the base
# image must survive every later definition; the magit bake reported such
# a binding "going missing" at a fixed coordinate, which was really the
# restore bootstrap above aborting before any extension ran.
parts_dir="$tmp_dir/parts"
mkdir -p "$parts_dir"
: > "$parts_dir/manifest.el"
for p in 0 1 2 3; do
  for i in $(seq 0 299); do
    printf '(defun nel-extend-part%d-f%d (x) (+ x %d))\n' "$p" "$i" "$i"
  done > "$parts_dir/part$p.el"
  printf '(load "%s" nil t)\n' "$parts_dir/part$p.el" >> "$parts_dir/manifest.el"
done
many_base="$tmp_dir/many-base.nlri"
many_out="$tmp_dir/many-out.nlri"
"$binary" dump-runtime-image "$many_base" \
  "(defun nel-extend-early (a b) (list a b))" >/dev/null
if ! "$binary" extend-runtime-image "$many_base" "$many_out" \
     "(load \"$parts_dir/manifest.el\" nil t)" \
     >"$tmp_dir/many.out" 2>"$tmp_dir/many.err"; then
  echo "standalone-extend-runtime-image-restore-order-test: FAIL -- many-defun extend aborted:" >&2
  cat "$tmp_dir/many.err" >&2
  exit 1
fi
actual="$("$binary" eval-runtime-image "$many_out" \
  "(list (fboundp 'nel-extend-early) (nel-extend-early 1 2) (nel-extend-part3-f299 1))")"
if [[ "$actual" != "(t (1 2) 300)" ]]; then
  echo "standalone-extend-runtime-image-restore-order-test: FAIL -- many-defun image result mismatch: $actual (expected (t (1 2) 300))" >&2
  exit 1
fi

# Negative control: a BASE-IMAGE path that does not exist must fail
# (exit 1) and must never write OUT-IMAGE.
set +e
"$binary" extend-runtime-image "$missing_base" "$missing_out" \
  "(setq nel-extend-restore-unreachable 1)" \
  >"$tmp_dir/missing.out" 2>"$tmp_dir/missing.err"
missing_rc=$?
set -e
if [[ "$missing_rc" -ne 1 ]]; then
  echo "standalone-extend-runtime-image-restore-order-test: FAIL -- missing-base exit=$missing_rc (expected 1)" >&2
  exit 1
fi
if [[ -e "$missing_out" ]]; then
  echo "standalone-extend-runtime-image-restore-order-test: FAIL -- missing-base wrote OUT-IMAGE anyway: $missing_out" >&2
  exit 1
fi

echo "standalone-extend-runtime-image-restore-order-test: PASS"
