#!/usr/bin/env bash
# Doc 47 Stage 9c — chain emit composition smoke.
#
# Drives `mint-int-plus-imm SRC IMM OUT' for a matrix of (heap_int,
# imm) pairs and asserts the boot exit code = heap_int + imm.  The
# emitted code segment is a single function body composed from three
# building blocks:
#
#   emit_load_heap_int_untag_head()   ; load argv[0] -> deref -> sar 3
#   emit_add_rax_imm32(IMM)            ; add rax, IMM (sign-extended)
#   emit_ret()                          ; ret
#
# Each pair exercises a different region of the i32 add-immediate
# space:
#   simple   : small positive heap + small positive imm
#   negative : positive heap + negative imm (subtract)
#   zero     : zero heap + small positive imm (= imm by itself)
#   recursion: heap from a defun-recursion eval (= proves SRC can be
#              non-trivial Elisp before lowering)
#   highbit  : heap = 0, imm = high-bit-set i32 (= sign-extension test)
set -euo pipefail
cd "$(git rev-parse --show-toplevel)"
cargo build --workspace --release --quiet
NELISP=target/release/nelisp
RUNTIME=target/release/nelisp-runtime

run_case() {
    local label="$1" src="$2" imm="$3" expected="$4"
    local out_bin="/tmp/doc47-stage9c-${label}.bin"
    rm -f "$out_bin"

    "$NELISP" mint-int-plus-imm "$src" "$imm" "$out_bin" >/dev/null

    local exit_code=0
    "$RUNTIME" boot-from-image "$out_bin" || exit_code=$?

    if [[ "$exit_code" -ne "$expected" ]]; then
        echo "FAIL ${label}: src='${src}' imm=${imm} expected=${expected} got=${exit_code}" >&2
        exit 1
    fi
    echo "OK ${label}: ${src} + ${imm} -> ${exit_code}"
}

# i32 high-bit case: 0 + (-1) = -1.  When boot returns -1 as i32, the
# OS exit code wraps to 255 (= 0xFF) per POSIX exit semantics.
run_case simple    "5"                                        10  15
run_case negative  "20"                                      -7   13
run_case zero      "0"                                        42  42
run_case recursion "(progn (defun f (n) (if (< n 2) n (+ (f (- n 1)) (f (- n 2))))) (f 6))" 8 16
run_case highbit   "0"                                       -1   255

echo
echo "Stage 9c chain emit composition: all cases passed."
