#!/bin/sh
set -eu

# Reusable S6 measurement harness (tools/ai/eln-progress.org, S6 section):
# for one genuine GNU .eln + its vendor Lisp source + a value corpus, run
# host GNU Emacs, the NeLisp VM (source evaluated, no native registration),
# and NeLisp's normal-load of the .eln through the admitted native path;
# compare all three exactly; time each with a 5-round minimum; and exit 0
# only if the three agree AND the native path actually made native calls.
#
# Usage:
#   NELISP_BIN=<bin> sh test/nelisp-eln-s6-measure.sh \
#     --eln PATH --function SYM --source PATH --corpus FILE
#
# Optional flags:
#   --timing-calls N     calls per timing round, per path (default 5; the
#                        current native registration path runs on the
#                        order of hundreds of milliseconds per call, so a
#                        large N makes the native phase impractically slow)
#   --wrapper FILE         a corpus wrapper file (test/fixtures/s6-corpus/),
#                          loaded identically after FUNCTION's own phase load
#                          in all three phases.  It must define a function
#                          named `s6-corpus--FUNCTION` (dashes as in
#                          FUNCTION) that binds whatever dynamic/file-local
#                          state FUNCTION needs and then calls FUNCTION *by
#                          symbol* so each phase's own definition runs; the
#                          corpus's argument lists are applied to that
#                          wrapper function instead of to FUNCTION directly.
#                          Use this for functions that read file-local
#                          bodyless-defvar specials (e.g. bytecomp.el's
#                          byte-compile--for-effect) that a bare `(apply
#                          FUNCTION args)` cannot supply; see
#                          test/fixtures/s6-corpus/README.md.  Admission
#                          (native-subr / registration) is still checked
#                          against FUNCTION itself, not the wrapper.
#   --inject-wrong        self-check only: corrupt the host phase's first
#                          result to prove the equality gate fails closed;
#                          never pass this for a real measurement
#   --log-root DIR         override the log directory (default
#                          $HOME/.cache/tmp/s6-measure; never /tmp)
#   --keep-logs 0|1        keep the per-run log directory (default 1)
#
# Prints one S6_MEASURE_RESULT line (key=value) to stdout and exits 0 only
# on PASS.

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repo=${NELISP_ROOT:-$(CDPATH= cd -- "$script_dir/.." && pwd)}
binary=${NELISP_BIN:-$repo/target/nelisp}
emacs_bin=${EMACS_BIN:-emacs}
driver=$script_dir/nelisp-eln-s6-measure-driver.el

eln=
function_name=
source_file=
corpus=
corpus_wrapper=
timing_calls=5
inject_wrong=0
log_root=${NELISP_ELN_S6_LOG_ROOT:-$HOME/.cache/tmp/s6-measure}
keep_logs=1
# --allow-stderr: judge phases by exit status and completion markers only,
# for functions whose genuine behaviour prints diagnostics (e.g. byte-compile
# warnings).  Off by default: unexpected stderr fails the phase.
allow_stderr=0

while [ $# -gt 0 ]; do
    case "$1" in
        --eln) eln=$2; shift 2 ;;
        --function) function_name=$2; shift 2 ;;
        --source) source_file=$2; shift 2 ;;
        --corpus) corpus=$2; shift 2 ;;
        --wrapper) corpus_wrapper=$2; shift 2 ;;
        --timing-calls) timing_calls=$2; shift 2 ;;
        --inject-wrong) inject_wrong=1; shift ;;
        --log-root) log_root=$2; shift 2 ;;
        --keep-logs) keep_logs=$2; shift 2 ;;
        --allow-stderr) allow_stderr=1; shift ;;
        *) echo "Unknown argument: $1" >&2; exit 2 ;;
    esac
done

# Host Emacs `load' does not resolve a relative path containing a
# directory against `default-directory', so every file argument is made
# absolute before any phase runs.
abspath() {
    case "$1" in
        '') printf '' ;;
        /*) printf '%s' "$1" ;;
        *) printf '%s/%s' "$(pwd)" "$1" ;;
    esac
}
eln=$(abspath "$eln")
source_file=$(abspath "$source_file")
corpus=$(abspath "$corpus")
corpus_wrapper=$(abspath "$corpus_wrapper")

if [ -z "$eln" ] || [ -z "$function_name" ] || [ -z "$source_file" ] || \
   [ -z "$corpus" ]; then
    echo "Usage: NELISP_BIN=<bin> sh $0 --eln PATH --function SYM --source PATH --corpus FILE" >&2
    exit 2
fi
case "$log_root" in
    /tmp|/tmp/*) echo "log-root must not be under /tmp: $log_root" >&2; exit 2 ;;
esac
if [ ! -x "$binary" ]; then
    echo "NELISP_BIN is not executable: $binary" >&2
    exit 2
fi
. "$script_dir/lib/nelisp-boot-args.sh"
nl_cold_image_setup "$binary" || exit 1
if ! command -v "$emacs_bin" >/dev/null 2>&1; then
    echo "EMACS_BIN is not executable: $emacs_bin" >&2
    exit 2
fi
for f in "$eln" "$source_file" "$corpus" "$driver"; do
    if [ ! -r "$f" ]; then
        echo "Not readable: $f" >&2
        exit 2
    fi
done
if [ -n "$corpus_wrapper" ] && [ ! -r "$corpus_wrapper" ]; then
    echo "Not readable: $corpus_wrapper" >&2
    exit 2
fi

mkdir -p "$log_root"
out_dir=$(mktemp -d "$log_root/${function_name}.XXXXXX")
if [ "$keep_logs" != 1 ]; then
    trap 'rm -rf "$out_dir"' EXIT HUP INT TERM
fi

cd "$repo"
shared_lisp=$repo/lisp
ffi_src=$repo/packages/nl-ffi/src

before=$(sha256sum "$eln" | cut -d ' ' -f 1)

# Generate the normal-load wrapper the same way
# test/nelisp-eln-same-artifact-smoke.sh does, by calling the standalone
# build script's own generator rather than reimplementing it.
wrapper=$out_dir/normal-load-wrapper.el
if ! "$emacs_bin" --batch -Q -L scripts -L lisp --eval \
    '(progn (defvar nelisp-standalone--repo-root (file-name-as-directory default-directory)) (dolist (name (list "nelisp-standalone--core-bytecode-src" "nelisp-standalone--after-load-runtime-src")) (with-temp-buffer (insert-file-contents "scripts/nelisp-standalone-build.el") (goto-char (point-min)) (unless (search-forward (concat "(defun " name) nil t) (error "source generator not found: %s" name)) (goto-char (match-beginning 0)) (eval (read (current-buffer))))) (princ (nelisp-standalone--after-load-runtime-src)))' \
    >"$wrapper" 2>"$out_dir/wrapper.stderr"; then
    cat "$out_dir/wrapper.stderr" >&2
    exit 1
fi

result_status=PASS
result_reason=

run_phase () {
    phase=$1
    shift
    NELISP_ELN_S6_PHASE=$phase \
    NELISP_ELN_S6_FUNCTION=$function_name \
    NELISP_ELN_S6_CORPUS=$corpus \
    NELISP_ELN_S6_ELN=$eln \
    NELISP_ELN_S6_SOURCE=$source_file \
    NELISP_ELN_S6_WRAPPER=$corpus_wrapper \
    NELISP_ELN_S6_TIMING_CALLS=$timing_calls \
    NELISP_ELN_S6_INJECT_WRONG=$([ "$phase" = host ] && [ "$inject_wrong" = 1 ] && echo 1 || echo 0) \
    "$@" >"$out_dir/$phase.stdout" 2>"$out_dir/$phase.stderr"
}

host_ok=1
run_phase host "$emacs_bin" --batch -Q --load "$driver" || host_ok=0
if [ "$host_ok" != 1 ] || { [ "$allow_stderr" != 1 ] && [ -s "$out_dir/host.stderr" ]; } || \
   ! grep -Fq "S6_PHASE_DONE phase=host function=$function_name status=ok" \
      "$out_dir/host.stdout"; then
    cat "$out_dir/host.stdout"
    cat "$out_dir/host.stderr" >&2
    echo "Host phase did not complete cleanly for $function_name" >&2
    exit 1
fi
corpus_n=$(grep -c '^S6_RESULT ' "$out_dir/host.stdout")

vm_ok=1
run_phase vm "$binary" ${NL_COLD_IMAGE_PATH:+--cold-load-from "$NL_COLD_IMAGE_PATH"} -L "$shared_lisp" -L "$repo/src" -L "$ffi_src" \
    --load "$driver" || vm_ok=0
if [ "$vm_ok" != 1 ] || { [ "$allow_stderr" != 1 ] && [ -s "$out_dir/vm.stderr" ]; } || \
   ! grep -Fq "S6_PHASE_DONE phase=vm function=$function_name status=ok" \
      "$out_dir/vm.stdout"; then
    cat "$out_dir/vm.stdout"
    cat "$out_dir/vm.stderr" >&2
    echo "VM phase did not complete cleanly for $function_name" >&2
    exit 1
fi

native_ok=1
run_phase native "$binary" ${NL_COLD_IMAGE_PATH:+--cold-load-from "$NL_COLD_IMAGE_PATH"} -L "$shared_lisp" -L "$repo/src" -L "$ffi_src" \
    --load "$shared_lisp/nelisp-eln-native-subr.el" \
    --load "$shared_lisp/nelisp-eln-registration.el" \
    --load "$wrapper" \
    --load "$driver" || native_ok=0
after=$(sha256sum "$eln" | cut -d ' ' -f 1)
if [ "$before" != "$after" ]; then
    echo "The .eln artifact changed during measurement" >&2
    exit 1
fi
if [ "$native_ok" != 1 ] || { [ "$allow_stderr" != 1 ] && [ -s "$out_dir/native.stderr" ]; } || \
   ! grep -Fq "S6_PHASE_DONE phase=native function=$function_name status=ok" \
      "$out_dir/native.stdout"; then
    reason=not_admitted_no_native_calls
    detail=$(grep -o 'S6_NATIVE_STATUS=not_admitted reason=.*' \
             "$out_dir/native.stdout" "$out_dir/native.stderr" 2>/dev/null | tail -1)
    printf 'S6_MEASURE_RESULT function=%s status=FAIL reason=%s corpus_n=%s detail=%s log_dir=%s\n' \
        "$function_name" "$reason" "$corpus_n" "${detail:-none}" "$out_dir"
    exit 1
fi

native_calls=$(grep -o 'S6_NATIVE_CALLS raw=[0-9]* dispatch=[0-9]*' \
    "$out_dir/native.stdout" | tail -1)
raw_calls=$(printf '%s' "$native_calls" | sed -n 's/.*raw=\([0-9]*\).*/\1/p')
dispatch_calls=$(printf '%s' "$native_calls" | sed -n 's/.*dispatch=\([0-9]*\).*/\1/p')
if [ -z "$raw_calls" ] || [ -z "$dispatch_calls" ] || \
   [ "$((raw_calls + dispatch_calls))" -le 0 ]; then
    printf 'S6_MEASURE_RESULT function=%s status=FAIL reason=not_admitted_no_native_calls corpus_n=%s log_dir=%s\n' \
        "$function_name" "$corpus_n" "$out_dir"
    exit 1
fi

# Compare the three phases' per-entry captures exactly.
grep '^S6_RESULT ' "$out_dir/host.stdout" | sed 's/.*\(capture=.*\)/\1/' \
    >"$out_dir/host.captures"
grep '^S6_RESULT ' "$out_dir/vm.stdout" | sed 's/.*\(capture=.*\)/\1/' \
    >"$out_dir/vm.captures"
grep '^S6_RESULT ' "$out_dir/native.stdout" | sed 's/.*\(capture=.*\)/\1/' \
    >"$out_dir/native.captures"
if ! diff -q "$out_dir/host.captures" "$out_dir/vm.captures" >/dev/null; then
    result_status=FAIL
    result_reason=value_mismatch_host_vm
fi
if ! diff -q "$out_dir/host.captures" "$out_dir/native.captures" >/dev/null; then
    result_status=FAIL
    result_reason=${result_reason:+${result_reason}+}value_mismatch_host_native
fi

host_timing=$(grep '^S6_TIMING ' "$out_dir/host.stdout" | tail -1)
vm_timing=$(grep '^S6_TIMING ' "$out_dir/vm.stdout" | tail -1)
native_timing=$(grep '^S6_TIMING ' "$out_dir/native.stdout" | tail -1)
host_ns=$(printf '%s' "$host_timing" | sed -n 's/.*ns_per_call=\([0-9]*\).*/\1/p')
vm_ns=$(printf '%s' "$vm_timing" | sed -n 's/.*ns_per_call=\([0-9]*\).*/\1/p')
native_ns=$(printf '%s' "$native_timing" | sed -n 's/.*ns_per_call=\([0-9]*\).*/\1/p')

if [ "$result_status" = PASS ]; then
    printf 'S6_MEASURE_RESULT function=%s status=PASS corpus_n=%s host_ns_per_call=%s vm_ns_per_call=%s native_ns_per_call=%s native_raw_calls=%s native_dispatch_calls=%s eln_sha256=%s log_dir=%s\n' \
        "$function_name" "$corpus_n" "${host_ns:-0}" "${vm_ns:-0}" \
        "${native_ns:-0}" "$raw_calls" "$dispatch_calls" "$before" "$out_dir"
    exit 0
else
    printf 'S6_MEASURE_RESULT function=%s status=FAIL reason=%s corpus_n=%s host_ns_per_call=%s vm_ns_per_call=%s native_ns_per_call=%s native_raw_calls=%s native_dispatch_calls=%s eln_sha256=%s log_dir=%s\n' \
        "$function_name" "$result_reason" "$corpus_n" "${host_ns:-0}" \
        "${vm_ns:-0}" "${native_ns:-0}" "$raw_calls" "$dispatch_calls" \
        "$before" "$out_dir"
    exit 1
fi
