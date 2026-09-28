#!/bin/sh
# S4.6: end-to-end smoke for the genuine gnu-chain.eln admission plus the
# owner/exit-contract discipline of the new `Ffuncall' MANY hop.
#
# Section 1 (real, no mocking): admits the genuine gnu-chain.eln via
# `nelisp-eln-native-subr-chain-import-analysis' against real ELF/GOT
# metadata, rejects the genuine dynamic-binding negative control the same
# way, and confirms the generic `comp--register-subr' load path admits
# gnu-chain.eln as an arity-2 native subr (Doc 207; live execution is in
# test/nelisp-eln-nonlocal-chain-smoke.sh).
#
# Section 2: exercises the real `nelisp-eln-callable-import--call-chain'
# and `nelisp-eln-callable-import--dispatch' for every outcome (normal
# return, error, throw, quit), with G a real Lisp closure genuinely
# encoded/decoded through `nelisp-eln-objects', real callback-import
# frame stack, and real owner/activation/pending baseline bookkeeping.
# Only the unavoidable native/FFI floor is stood in for, using the same
# technique test/nelisp-eln-callable-import-test.el already uses for the
# existing unary/MANY lanes. Negative controls: the assertion helper
# itself rejects a normal return where a signal was wanted, and the
# baseline check itself detects a deliberately skipped release.
set -eu

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repo=${NELISP_ROOT:-$(CDPATH= cd -- "$script_dir/.." && pwd)}
binary=${NELISP_BIN:-$repo/target/nelisp}
emacs_bin=${EMACS_BIN:-emacs}
ffi_root=${NELISP_ELN_SYSTEM_LOADER_FFI_ROOT:-$repo}
shared_lisp=${NELISP_SHARED_LISP:-$repo/lisp}
out_dir=$(mktemp -d "${TMPDIR:-/tmp}/nelisp-eln-chain-unwind.XXXXXX")
provided_load_wrapper=${NELISP_ELN_LOAD_WRAPPER:-}
normal_load_wrapper=${provided_load_wrapper:-$out_dir/generated-wrapper.el}
keep_artifacts=${NELISP_ELN_KEEP_ARTIFACTS:-0}

cache_root_default=${NELISP_ELN_CACHE_ROOT:-$HOME/.cache}
chain_input=${NELISP_ELN_GNU_CHAIN_INPUT:-$cache_root_default/tmp/s46-chain-artifact/overlay/eln/31.1-ba35c031/gnu-chain.eln}
dynamic_input=${NELISP_ELN_GNU_CHAIN_DYNAMIC_INPUT:-$cache_root_default/tmp/s46-chain-artifact/overlay/dynamic-NEGATIVE-CONTROL/eln/31.1-ba35c031/gnu-chain-dynamic.eln}
driver=$script_dir/nelisp-eln-chain-unwind-driver.el

export NELISP_ROOT=$repo
export NELISP_ELN_SYSTEM_LOADER_SOURCE_ROOT=$repo
export NELISP_ELN_SYSTEM_LOADER_FFI_ROOT=$ffi_root
cd "$repo"
trap 'status=$?; if [ "$status" -eq 0 ] && [ "$keep_artifacts" != 1 ]; then rm -rf "$out_dir"; else echo "ARTIFACT_DIR=$out_dir" >&2; fi' EXIT HUP INT TERM

if [ ! -x "$binary" ]; then
    echo "NELISP_BIN is not executable: $binary" >&2
    exit 2
fi
if [ ! -r "$chain_input" ] || [ ! -r "$dynamic_input" ]; then
    echo "NELISP_ELN_GNU_CHAIN_INPUT and NELISP_ELN_GNU_CHAIN_DYNAMIC_INPUT must name readable gnu-chain artifacts" >&2
    exit 2
fi
if [ ! -r "$driver" ]; then
    echo "chain-unwind driver missing: $driver" >&2
    exit 2
fi
if [ -z "$provided_load_wrapper" ]; then
    if ! "$emacs_bin" --batch -Q -L scripts -L lisp --eval \
        '(progn (defvar nelisp-standalone--repo-root (file-name-as-directory default-directory)) (dolist (name (list "nelisp-standalone--core-bytecode-src" "nelisp-standalone--after-load-runtime-src")) (with-temp-buffer (insert-file-contents "scripts/nelisp-standalone-build.el") (goto-char (point-min)) (unless (search-forward (concat "(defun " name) nil t) (error "source generator not found: %s" name)) (goto-char (match-beginning 0)) (eval (read (current-buffer))))) (princ (nelisp-standalone--after-load-runtime-src)))' \
        >"$normal_load_wrapper" 2>"$out_dir/wrapper.stderr"; then
        cat "$out_dir/wrapper.stderr" >&2
        exit 1
    fi
fi
if [ ! -r "$normal_load_wrapper" ]; then
    echo "ELN load wrapper is not readable: $normal_load_wrapper" >&2
    exit 2
fi

gnu_before=$(sha256sum "$chain_input" | cut -d ' ' -f 1)
dynamic_before=$(sha256sum "$dynamic_input" | cut -d ' ' -f 1)

export NELISP_ELN_GNU_INPUT=$chain_input
export NELISP_ELN_GNU_DYNAMIC_INPUT=$dynamic_input
if ! "$binary" -L "$shared_lisp" -L "$repo/src" -L "$repo/packages/nl-ffi/src" \
    --load "$shared_lisp/nelisp-eln-native-subr.el" \
    --load "$shared_lisp/nelisp-eln-registration.el" \
    --load "$normal_load_wrapper" --load "$driver" \
    >"$out_dir/chain-unwind.stdout" 2>"$out_dir/chain-unwind.stderr"; then
    cat "$out_dir/chain-unwind.stdout"
    cat "$out_dir/chain-unwind.stderr" >&2
    exit 1
fi

if [ -s "$out_dir/chain-unwind.stderr" ]; then
    cat "$out_dir/chain-unwind.stdout"
    cat "$out_dir/chain-unwind.stderr" >&2
    echo "S4.6 chain-unwind smoke produced unexpected stderr" >&2
    exit 1
fi

for pattern in \
    'NELISP_CHAIN_STAGE=admit_genuine_chain_slots_1301_945' \
    'NELISP_CHAIN_STAGE=reject_dynamic_binding_control' \
    'NELISP_CHAIN_STAGE=registration_admits_chain_via_generic_load' \
    'NELISP_CHAIN_STAGE=self_check_assertion_helper_rejects_wrong_outcome=PASS' \
    'NELISP_CHAIN_STAGE=outcome_normal_chain_calls_1_decrement_calls_1_baseline_restored=PASS' \
    'NELISP_CHAIN_STAGE=outcome_error_chain_calls_1_decrement_calls_0_baseline_restored=PASS' \
    'NELISP_CHAIN_STAGE=outcome_throw_chain_calls_1_decrement_calls_0_baseline_restored=PASS' \
    'NELISP_CHAIN_STAGE=outcome_quit_chain_calls_1_decrement_calls_0_baseline_restored=PASS' \
    'NELISP_CHAIN_STAGE=self_check_baseline_detects_skipped_release=PASS' \
    'NELISP-ELN-S46-CHAIN-UNWIND-PASS'; do
    if ! grep -Fx "$pattern" "$out_dir/chain-unwind.stdout" >/dev/null; then
        cat "$out_dir/chain-unwind.stdout"
        echo "S4.6 chain-unwind smoke missing expected line: $pattern" >&2
        exit 1
    fi
done

gnu_after=$(sha256sum "$chain_input" | cut -d ' ' -f 1)
dynamic_after=$(sha256sum "$dynamic_input" | cut -d ' ' -f 1)
if [ "$gnu_before" != "$gnu_after" ] || [ "$dynamic_before" != "$dynamic_after" ]; then
    echo "Admission changed a genuine GNU chain artifact" >&2
    exit 1
fi

printf 'NELISP-ELN-GNU-CHAIN-UNWIND-PASS %s\n' "$gnu_before"
exit 0
