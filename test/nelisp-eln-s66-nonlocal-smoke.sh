#!/bin/sh
# S6.6: dynamic binding (`specbind' / `helper_unbind_n') of the genuine
# gnu-cconv-closure-convert.eln is restored on every exit -- normal, error,
# throw, quit, nested and unbound-before -- exactly as host GNU Emacs 31.1
# running the same artifact natively does.  Both runtimes run
# test/nelisp-eln-s66-nonlocal-driver.el; their `S66 ' transcripts must be
# identical, NeLisp's stderr must be empty and the driver must finish.
set -eu

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repo=${NELISP_ROOT:-$(CDPATH= cd -- "$script_dir/.." && pwd)}
binary=${NELISP_BIN:-$repo/target/nelisp}
host_emacs=${NELISP_S66_HOST_EMACS:-${EMACS_BIN:-emacs}}
eln=${S66_ELN:-$HOME/.cache/tmp/s6-survey-lex/cconv-closure-convert/overlay/eln/31.1-ba35c031/gnu-cconv-closure-convert.eln}
driver=$script_dir/nelisp-eln-s66-nonlocal-driver.el
out_dir=$(mktemp -d "${TMPDIR:-$HOME/.cache/tmp}/nelisp-eln-s66.XXXXXX")
trap 'status=$?; if [ "$status" -eq 0 ]; then rm -rf "$out_dir"; else echo "ARTIFACT_DIR=$out_dir" >&2; fi' EXIT HUP INT TERM

[ -x "$binary" ] || { echo "NELISP_BIN is not executable: $binary" >&2; exit 2; }
[ -r "$eln" ] || { echo "S6.6 artifact is not readable: $eln" >&2; exit 2; }
case "$("$host_emacs" --version 2>/dev/null | head -n 1)" in
    "GNU Emacs 31.1"*) ;;
    *) echo "host emacs must be GNU Emacs 31.1 (ABI ba35c031)" >&2; exit 2 ;;
esac
export S66_ELN=$eln

"$host_emacs" --batch -Q -l "$driver" > "$out_dir/host.out" 2> "$out_dir/host.err" \
    || { echo "S6.6 host driver failed" >&2; tail -n 5 "$out_dir/host.err" >&2; exit 1; }
(cd "$out_dir" && "$binary" --load "$driver" -- > nelisp.out 2> nelisp.err) \
    || { echo "S6.6 NeLisp driver failed" >&2; tail -n 5 "$out_dir/nelisp.err" >&2; exit 1; }
grep '^S66 ' "$out_dir/host.out" > "$out_dir/host.s66"
grep '^S66 ' "$out_dir/nelisp.out" > "$out_dir/nelisp.s66"
if [ -s "$out_dir/nelisp.err" ]; then
    echo "S6.6 NeLisp wrote to stderr:" >&2; head -n 5 "$out_dir/nelisp.err" >&2; exit 1
fi
if ! tail -n 1 "$out_dir/nelisp.s66" | grep -qx 'S66 done'; then
    echo "S6.6 NeLisp driver did not finish" >&2; exit 1
fi
if ! diff "$out_dir/host.s66" "$out_dir/nelisp.s66" > "$out_dir/diff.txt"; then
    echo "S6.6 host/NeLisp transcripts differ:" >&2; head -n 20 "$out_dir/diff.txt" >&2; exit 1
fi
echo "S66_NONLOCAL_RESULT status=PASS lines=$(wc -l < "$out_dir/nelisp.s66")"
