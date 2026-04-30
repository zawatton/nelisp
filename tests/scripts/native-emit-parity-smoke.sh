#!/usr/bin/env bash
# Doc 47 Stage 9b — native emit parity smoke.
#
# Mints the same Elisp expression two ways and asserts:
#   1. both images boot to the same exit code (= functional parity)
#   2. both images carry byte-identical code segments
#      (= the build-tool's emit_load_heap_int_untag matches the
#         runtime's pre-baked NATIVE_LOAD_HEAP_INT_UNTAG asset)
#
#   path A: mint-eval-result
#       → bundles the runtime's pre-baked NATIVE_LOAD_HEAP_INT_UNTAG
#         asset bytes verbatim
#   path B: mint-int-via-emitted-load
#       → calls native_emit::emit_load_heap_int_untag() and writes
#         that result into the same code-segment slot
#
# The bytes equality is a Stage 9c precondition: closure-body
# compilation can only treat its building blocks as composable if a
# single hand-emitted block is byte-equal to the asset the runtime
# would have linked itself.
set -euo pipefail
cd "$(git rev-parse --show-toplevel)"
cargo build --workspace --release --quiet
NELISP=target/release/nelisp
RUNTIME=target/release/nelisp-runtime

run_pair() {
    local label="$1" expr="$2" expected="$3"
    local asset_bin="/tmp/doc47-stage9b-${label}-asset.bin"
    local emitted_bin="/tmp/doc47-stage9b-${label}-emitted.bin"
    rm -f "$asset_bin" "$emitted_bin"

    "$NELISP" mint-eval-result          "$expr" "$asset_bin"   >/dev/null
    "$NELISP" mint-int-via-emitted-load "$expr" "$emitted_bin" >/dev/null

    local asset_exit=0 emitted_exit=0
    "$RUNTIME" boot-from-image "$asset_bin"   || asset_exit=$?
    "$RUNTIME" boot-from-image "$emitted_bin" || emitted_exit=$?

    if [[ "$asset_exit" -ne "$expected" ]]; then
        echo "FAIL ${label}: asset path exit=${asset_exit} expected=${expected}" >&2
        exit 1
    fi
    if [[ "$emitted_exit" -ne "$expected" ]]; then
        echo "FAIL ${label}: emitted path exit=${emitted_exit} expected=${expected}" >&2
        exit 1
    fi

    # Code-segment slice = bytes [8192, 8192+11) per Doc 47 image
    # layout (page-aligned heap, code immediately after).  The slice
    # offset is fixed by the dumper — we just compare full files
    # byte-for-byte; equality is a stronger guarantee than the slice.
    if ! cmp --silent "$asset_bin" "$emitted_bin"; then
        echo "FAIL ${label}: image bytes differ between asset and emitted paths" >&2
        echo "  asset:   $(sha256sum "$asset_bin" | cut -d' ' -f1)" >&2
        echo "  emitted: $(sha256sum "$emitted_bin" | cut -d' ' -f1)" >&2
        diff <(xxd "$asset_bin") <(xxd "$emitted_bin") | head -20 >&2
        exit 1
    fi

    echo "OK ${label}: exit=${asset_exit} (both paths) image_bytes_equal=yes"
}

run_pair simple    "(+ 1 2 3)"                        6
run_pair recursion "(progn (defun f (n) (if (< n 2) n (+ (f (- n 1)) (f (- n 2))))) (f 7))" 13
run_pair zero      "0"                                0

echo
echo "Stage 9b native emit parity: all cases byte-identical."
