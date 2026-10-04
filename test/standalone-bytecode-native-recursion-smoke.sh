#!/usr/bin/env bash
set -euo pipefail
script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
repo_root=$(cd -- "$script_dir/.." && pwd)
binary=${NELISP_BINARY:?set NELISP_BINARY to the pinned runtime}
[[ -x "$binary" ]] || { echo 'recursive smoke: binary not executable' >&2; exit 1; }
binary=$(realpath "$binary")
sha=$(sha256sum "$binary" | awk '{print $1}')
expected=${RECURSION_EXPECTED_SHA256:-4cba6d8a65463a43ae825e7f46ca417fd7c9825efc949b8906cdd62296deef05}
[[ "$sha" == "$expected" ]] || { echo 'recursive smoke: runtime SHA mismatch' >&2; exit 1; }
emacs_cmd=${EMACS:-emacs}
[[ "$($emacs_cmd --batch -Q --eval '(princ emacs-version)')" == 31.1* ]] || {
  echo 'recursive smoke requires GNU Emacs 31.1' >&2; exit 1;
}
task_tmp="$repo_root/target/progress/recursive-call1-cons-tmp"
mkdir -p "$task_tmp"
tmpdir=$(mktemp -d "$task_tmp/run.XXXXXX")
record_dir="$repo_root/target/progress/recursive-call1-cons-records/$(date +%Y%m%dT%H%M%S)-$$"
mkdir -p "$record_dir"
trap 'rm -rf -- "$tmpdir"' EXIT
source="$tmpdir/recursive.el"
elc="$tmpdir/recursive.elc"
cp "$script_dir/fixtures/native-bytecode/gnu-recursive-call1-cons-31.1.el" "$source"
export RECURSION_SOURCE="$source"
cat >"$tmpdir/compile.el" <<'ELISP'
;;; -*- lexical-binding: t; -*-
(princ "checkpoint:compile-start\n")
(unless (byte-compile-file (getenv "RECURSION_SOURCE"))
  (error "GNU byte compilation failed"))
(princ "checkpoint:compile-done\n")
ELISP
"$emacs_cmd" --batch -Q --load "$tmpdir/compile.el"
[[ -s "$elc" ]]
rm "$source"
[[ ! -e "$source" ]]
export RECURSION_ELC="$elc"
oracle=$(timeout 30s "$emacs_cmd" --batch -Q \
  --load "$script_dir/fixtures/native-bytecode/gnu-recursive-call1-cons-oracle.el" \
  2>"$tmpdir/oracle.err")
grep -Fq 'GNU-ORACLE-PASS calls=5' "$tmpdir/oracle.err"
export RECURSION_ELC="$elc" RECURSION_ORACLE="$oracle"
export RECURSION_CALL1_ARTIFACT="$tmpdir/call1.nelr"
export RECURSION_CONS_ARTIFACT="$tmpdir/cons.nelr"
export RECURSION_BINARY_SHA="$sha"
cat >"$tmpdir/runtime-probe.el" <<ELISP
(princ "checkpoint:runtime-abi-start\\n")
(require 'nelisp-runtime-reload-abi)
(princ "checkpoint:runtime-abi-loaded\\n")
(require 'nelisp-bytecode-native-package)
(princ "checkpoint:package-loaded\\n")
(require 'nelisp-bytecode-native-compiler)
(princ "checkpoint:compiler-loaded\\n")
(require 'nelisp-native-load)
(princ "checkpoint:native-load-loaded\\n")
(princ "checkpoint:driver-load-start\\n")
(load "$script_dir/standalone-bytecode-native-recursion-driver.el")
ELISP
set +e
timeout 180s "$binary" -L "$repo_root/lisp" -L "$repo_root/src" \
  -L "$repo_root/scripts" -L "$repo_root/packages/nl-prelude/src" \
  --load "$tmpdir/runtime-probe.el" \
  >"$tmpdir/out" 2>"$tmpdir/err"
status=$?
set -e
cp "$tmpdir/out" "$record_dir/stdout"
cp "$tmpdir/err" "$record_dir/stderr"
printf '%s\n' "$status" >"$record_dir/exit-status"
printf '%s\n' "$sha" >"$record_dir/runtime-sha256"
sha256sum "$record_dir/stdout" "$record_dir/stderr" >"$record_dir/output-sha256"
if [[ $status -ne 0 ]] || [[ -s "$tmpdir/err" ]] || \
   ! grep -Fxq 'CALL1-VM-RECURSIVE-CONS-GC-PASS' "$tmpdir/out"; then
  cat "$tmpdir/out" >&2; cat "$tmpdir/err" >&2; exit 1
fi
printf '%s\n' 'CALL1-VM-RECURSIVE-CONS-GC-PASS' >"$record_dir/completion-marker"
echo "recursive-call1-cons: PASS (runtime exit $status; source removed; GNU oracle matched)"
echo "record: $record_dir"
