#!/bin/sh
# Build parity for the checked-in generated Lisp unit. This does not lower .nl.
set -eu

root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
baseline=${NELISP_BASELINE_BIN:?set NELISP_BASELINE_BIN to the fixed baseline executable}
artifacts=${NELISP_ARTIFACT_DIR:?set NELISP_ARTIFACT_DIR to an isolated output directory}
emacs=${EMACS:-emacs}
candidate="$artifacts/nelisp-rebuilt"
candidate=${NELISP_CANDIDATE_BIN:-$candidate}
mkdir -p "$artifacts"

hash_file() { sha256sum "$1" | awk '{print $1}'; }
baseline_hash=$(hash_file "$baseline")
nl="$root/packages/nelisp-sys/eval-port/env-leaves-bind.nl"
generated=${NELISP_GENERATED_SOURCE:-$root/lisp/nelisp-cc-evalport-env-leaves-bind.el}
source_manifest() {
    (cd "$root" && find . -type f ! -path './.git/*' ! -path './target/*' \
        ! -name '*.elc' -print0 | sort -z | xargs -0 sha256sum)
}

grep -q '^(sys:defun nl_bf_bind_sym' "$nl" || {
    echo "FAIL: missing DSL entrypoint in $nl" >&2; exit 1;
}
grep -q '(defun nl_bf_bind_sym' "$generated" || {
    echo "FAIL: missing generated entrypoint in $generated" >&2; exit 1;
}
if grep -q 'nl_bind_frame_fast' "$nl"; then
    dsl_fast=yes
else
    dsl_fast=no
fi
if grep -q '(defun nl_bind_frame_fast' "$generated" && grep -q '(nl_bind_frame_fast frames_ptr name_ptr val_ptr)' "$generated"; then
    generated_fast=yes
else
    generated_fast=no
fi
[ "$generated_fast" = yes ] || {
    echo 'FAIL: generated Lisp is missing nl_bind_frame_fast definition/call' >&2; exit 1;
}
if [ "$dsl_fast" != "$generated_fast" ]; then
    echo "DRIFT: DSL nl_bind_frame_fast=$dsl_fast; generated Lisp helper/call=$generated_fast"
    echo 'SCOPE: generated-source build parity only; .nl lowering is unavailable in this build path.'
fi

source_manifest > "$artifacts/source-before.sha256"
if [ -z "${NELISP_CANDIDATE_BIN:-}" ]; then
    NELISP_STANDALONE_READER_OUTPUT="$candidate" make -C "$root" standalone-reader
fi
[ -x "$candidate" ] || { echo "FAIL: missing candidate $candidate" >&2; exit 1; }
host_version=$($emacs --version | sed -n '1p')
[ "$host_version" = 'GNU Emacs 31.1' ] || {
    echo "FAIL: expected GNU Emacs 31.1, got $host_version" >&2; exit 1;
}

probe_ok() {
    label=$1 form=$2 expected=$3
    host=$($emacs -Q --batch --eval "(princ (format \"%S\" $form))")
    base=$($baseline --eval "$form")
    rebuilt=$($candidate --eval "$form")
    if [ "$host" != "$expected" ] || [ "$base" != "$expected" ] || [ "$rebuilt" != "$expected" ]; then
        echo "FAIL: $label host=<$host> baseline=<$base> rebuilt=<$rebuilt> expected=<$expected>" >&2; exit 1
    fi
    echo "PASS: $label host, baseline and rebuilt agree: <$expected>"
}
probe_arity_error() {
    label=$1 form=$2
    host=$($emacs -Q --batch --eval "(condition-case e (progn $form (princ \"no-error\")) (error (princ (symbol-name (car e)))))")
    [ "$host" = wrong-number-of-arguments ] || { echo "FAIL: $label Host returned <$host>" >&2; exit 1; }
    for executable in "$baseline" "$candidate"; do
        if output=$("$executable" --eval "$form" 2>&1); then
            echo "FAIL: $label unexpectedly succeeded: $output" >&2; exit 1
        fi
        case "$output" in *wrong-number-of-arguments*) ;; *)
            echo "FAIL: $label first error was: $output" >&2; exit 1;; esac
    done
    echo "PASS: $label Host, baseline and rebuilt signal wrong-number-of-arguments"
}

probe_ok startup '(+ 40 2)' 42
probe_arity_error too-few '((lambda (x) x))'
probe_arity_error too-many '((lambda (x) x) 1 2)'
probe_ok dynamic-binding "(progn (makunbound 'envbind-parity-special) (defvar envbind-parity-special) (list (let ((envbind-parity-special 17)) envbind-parity-special) (boundp 'envbind-parity-special)))" "(17 nil)"
probe_ok lexical-binding-after-gc '(let ((envbind-parity-local (cons 1 2))) (garbage-collect) envbind-parity-local)' "(1 . 2)"
probe_ok dynamic-binding-after-gc "(progn (makunbound 'envbind-parity-gc-special) (defvar envbind-parity-gc-special) (let ((envbind-parity-gc-special 19)) (garbage-collect) envbind-parity-gc-special))" 19

[ "$(hash_file "$baseline")" = "$baseline_hash" ] || { echo 'FAIL: baseline binary changed during comparison' >&2; exit 1; }
source_manifest > "$artifacts/source-after.sha256"
cmp -s "$artifacts/source-before.sha256" "$artifacts/source-after.sha256" || {
    echo 'FAIL: source tree changed during build/parity run' >&2
    diff -u "$artifacts/source-before.sha256" "$artifacts/source-after.sha256" >&2 || :
    exit 1
}
printf '%s\n' "baseline_sha256=$baseline_hash" "candidate_sha256=$(hash_file "$candidate")" \
    "dsl_fast_path=$dsl_fast" "generated_fast_path=$generated_fast" > "$artifacts/identity.txt"
echo "PASS: generated-source build parity; baseline=$(hash_file "$baseline") candidate=$(hash_file "$candidate")"
