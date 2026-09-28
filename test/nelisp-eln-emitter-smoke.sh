#!/bin/sh
set -eu

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repo=${NELISP_ROOT:-$(CDPATH= cd -- "$script_dir/.." && pwd)}
binary=${NELISP_BIN:-$repo/target/nelisp}
out_dir=$(mktemp -d "${TMPDIR:-/tmp}/nelisp-eln-emitter.XXXXXX")
export NELISP_ROOT=$repo
export NELISP_ELN_OUT=$out_dir/constant17.eln
trap 'rm -rf "$out_dir"' EXIT HUP INT TERM

if [ ! -x "$binary" ]; then
    echo "NELISP_BIN is not executable: $binary" >&2
    exit 2
fi

cd "$repo"
if ! "$binary" -L "$repo/lisp" -L "$script_dir/../lisp" \
    --eval '(progn (require (quote nelisp-eln-emitter)) (let ((ir (nelisp-aot-compiler--parse-stmt (quote (defun nelisp-eln-emitter-standalone-smoke (value) value)) nil nil nil))) (nelisp-eln-emitter-write-ir ir (getenv "NELISP_ELN_OUT"))) (princ "NELISP-ELN-EMIT-PASS\n"))' \
    >"$out_dir/nelisp.stdout" 2>"$out_dir/nelisp.stderr"; then
    cat "$out_dir/nelisp.stdout"
    cat "$out_dir/nelisp.stderr" >&2
    exit 1
fi
if [ -s "$out_dir/nelisp.stderr" ] || ! grep -q 'NELISP-ELN-EMIT-PASS' "$out_dir/nelisp.stdout"; then
    cat "$out_dir/nelisp.stdout"
    cat "$out_dir/nelisp.stderr" >&2
    echo "standalone emitter did not complete cleanly" >&2
    exit 1
fi
before=$(sha256sum "$NELISP_ELN_OUT" | cut -d ' ' -f 1)

if ! emacs -Q --batch -l "$script_dir/../test/nelisp-eln-emitter-host-smoke.el" \
    >"$out_dir/host.stdout" 2>"$out_dir/host.stderr"; then
    cat "$out_dir/host.stdout"
    cat "$out_dir/host.stderr" >&2
    exit 1
fi
after=$(sha256sum "$NELISP_ELN_OUT" | cut -d ' ' -f 1)
if [ -s "$out_dir/host.stderr" ] || ! grep -q 'GNU-ELN-LOAD-PASS' "$out_dir/host.stdout" || [ "$before" != "$after" ]; then
    cat "$out_dir/host.stdout"
    cat "$out_dir/host.stderr" >&2
    echo "GNU loader smoke failed or changed the emitted artifact" >&2
    exit 1
fi
printf 'NELISP-ELN-EMITTER-SMOKE-PASS %s\n' "$before"
