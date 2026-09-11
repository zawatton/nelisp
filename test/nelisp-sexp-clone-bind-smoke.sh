#!/bin/sh
# nelisp-sexp-clone-bind-smoke.sh --- regression check for the reader-
# boundary reclaim / bind-scratch corruption fixed in
# `nl_boundary_reclaim' / `nl_boundary_reset_tail_chunks'
# (scripts/nelisp-standalone-build.el).
#
# THE DEFECT (fixed 2026-09-12).  `nl_eval_source_all''s per-top-level-form
# loop takes a bump-arena "mark" (chunk + cursor) BEFORE reading the form's
# text, then -- after that form has been fully read AND evaluated -- calls
# `nl_boundary_maybe_reclaim' with that same mark.  When the form's own
# result turns out to be an immediate Sexp (Nil/T/Int/Float) and the
# mutation epoch never moved, `nl_boundary_reclaim' rewinds the chunk's bump
# cursor straight back to the mark, handing that whole span back to the
# ordinary bump allocator for reuse -- WITHOUT zeroing it, unlike the
# free-list reuse path (`nl_alloc_bytes_uncheck' + `nl_alloc_zero_fill').
# The rewound span had already been written to (it is exactly what the
# reader used to build that form's own parse tree, plus whatever it
# evaluated), so it is not the "freshly bumped, OS-zero-filled page" every
# constructor in this codebase assumes.
#
# The next top-level form's `nl_bind_frame_fast' (lisp/nelisp-cc-evalport-
# env-leaves-bind.el) allocates a 128-byte scratch block for binding a
# builtin's own formal parameters and can land inside that exact rewound
# span.  `nelisp_frame_bind_prepend' (lisp/nelisp-cc-frame-bind.el)
# unconditionally trusts one 32-byte slice of that scratch block to be a
# caller-zeroed `Sexp::Nil' and uses it as the `cons-make' seed for the
# fresh (NAME . CELL) pair it is about to build.  When the stale bytes left
# there decode as `Sexp::Cons' (tag 7) with a garbage payload pointer,
# cloning it (`nl_val_clone_into' -> `nl_sexp_clone_into' -> `nl_sci_rc' ->
# `nl_sci_bump' -> `nelisp_nlconsbox_clone') dereferences the garbage
# pointer: SIGSEGV.
#
# SENSITIVITY, why this file varies the `condition-case' VARIABLE name
# length.  The defect is a heap-timing bug, not a fixed-size buffer -- but a
# `condition-case' handler variable of 9+ bytes (over one 64-bit word) vs.
# 8 or fewer shifts how many bytes the reader consumes materialising that
# symbol, which shifts whether the later `nl_bind_frame_fast' scratch block
# lands back inside the exact span `nl_boundary_reclaim' rewound.  Measured:
# names of 8 bytes or fewer never crashed; 9 or more always did, on every
# affected build.  This smoke exercises BOTH sides of that boundary so a
# regression that narrows or widens the exposure window is caught at the
# mechanism, not only at one fixed reproducer string.
#
# Each case below is the same three-line program that reproduced the bug:
#   (princ "MARK\n")
#   (condition-case VARNAME (when (require 'zzz-absent nil t)
#                              (nelix-package-activate-emacs))
#     (t (princ "H\n")))
#   (princ "REACHED_END\n")
# `require' returns nil (feature absent, NOERROR=t) without signalling, so
# `when' never runs its body and the handler clause never fires either --
# the crash was never about condition-case actually catching anything.

set -u

smoke_script_dir=$(cd "$(dirname "$0")" && pwd)
smoke_root=$(cd "$smoke_script_dir/.." && pwd)
cd "$smoke_root" || exit 1

smoke_bin=${NELISP_BIN:-}
if [ -z "$smoke_bin" ]; then
    for smoke_candidate in target/nelisp.exe target/nelisp; do
        if [ -f "$smoke_candidate" ]; then
            smoke_bin=$smoke_candidate
            break
        fi
    done
fi
if [ -z "$smoke_bin" ] || [ ! -f "$smoke_bin" ]; then
    echo "GATE-SKIP no nelisp binary in target/ (build with: make standalone-reader) and NELISP_BIN unset"
    echo "GATE-COUNT checked=0 findings=0"
    exit 0
fi
if [ ! -x "$smoke_bin" ]; then
    chmod +x "$smoke_bin" 2>/dev/null || true
fi

# A cross-compiled binary that cannot execute on this host is a skip, not a
# failure: probe with the smallest possible invocation before spending any
# real cases on it.
if ! "$smoke_bin" --eval '(+ 1 1)' >/dev/null 2>&1; then
    echo "GATE-SKIP $smoke_bin is not runnable on this host"
    echo "GATE-COUNT checked=0 findings=0"
    exit 0
fi

smoke_dir=$(mktemp -d "${TMPDIR:-/tmp}/nelisp-sexp-clone-bind-smoke.XXXXXX")
smoke_keep=${NELISP_SEXP_CLONE_BIND_SMOKE_KEEP:-0}
smoke_status=0
smoke_findings=0
smoke_checked=0
cleanup_smoke() {
    trap - EXIT
    if [ "$smoke_keep" = 1 ]; then
        printf 'smoke evidence retained: %s\n' "$smoke_dir" >&2
    else
        rm -rf "$smoke_dir"
    fi
}
trap cleanup_smoke EXIT

# run_case NAME VARNAME QUOTE_STYLE EXPECT_REACHED
#   QUOTE_STYLE: "shorthand" for 'zzz-absent, "explicit" for (quote zzz-absent).
#   EXPECT_REACHED: "yes" -- every case here is expected to reach the end
#   and exit 0 post-fix; kept as a parameter so a future case that is
#   SUPPOSED to still fail some other way (not this defect) can say so.
run_case() {
    case_name=$1
    varname=$2
    quote_style=$3
    smoke_checked=$((smoke_checked + 1))

    case_file="$smoke_dir/$case_name.el"
    if [ "$quote_style" = "shorthand" ]; then
        quoted_feature="'zzz-absent"
    else
        quoted_feature="(quote zzz-absent)"
    fi
    cat > "$case_file" <<EOF
(princ "MARK\n")
(condition-case $varname (when (require $quoted_feature nil t) (nelix-package-activate-emacs)) (t (princ "H\n")))
(princ "REACHED_END\n")
EOF

    case_out="$smoke_dir/$case_name.out"
    "$smoke_bin" --load "$case_file" > "$case_out" 2>&1
    case_rc=$?

    if [ "$case_rc" = 139 ] || [ "$case_rc" -gt 128 ]; then
        echo "nelisp-sexp-clone-bind-smoke: $case_name (varname=$varname len=${#varname} quote=$quote_style) CRASHED, exit $case_rc" >&2
        cat "$case_out" >&2 || true
        smoke_findings=$((smoke_findings + 1))
        return
    fi
    if [ "$case_rc" != 0 ]; then
        echo "nelisp-sexp-clone-bind-smoke: $case_name (varname=$varname len=${#varname} quote=$quote_style) exited $case_rc (expected 0)" >&2
        cat "$case_out" >&2 || true
        smoke_findings=$((smoke_findings + 1))
        return
    fi
    if ! grep -Fq 'MARK' "$case_out" || ! grep -Fq 'REACHED_END' "$case_out"; then
        echo "nelisp-sexp-clone-bind-smoke: $case_name (varname=$varname len=${#varname} quote=$quote_style) missing MARK/REACHED_END in output" >&2
        cat "$case_out" >&2 || true
        smoke_findings=$((smoke_findings + 1))
        return
    fi
    echo "nelisp-sexp-clone-bind-smoke: $case_name (varname=$varname len=${#varname} quote=$quote_style) PASS"
}

# --- Boundary: variable name length, quote shorthand (the original
# reproducer's own shape). ---
run_case "len6-ok"        "aaaaaa"    shorthand
run_case "len8-boundary"  "aaaaaaaa"  shorthand
run_case "len9-boundary"  "aaaaaaaaa" shorthand
run_case "len15-over"     "aaaaaaaaaaaaaaa" shorthand

# --- Same boundary with the explicit (quote zzz-absent) form, to keep the
# reader's-alternate-allocation-shape sensitivity covered too. ---
run_case "len8-explicit-quote"  "aaaaaaaa"  explicit
run_case "len9-explicit-quote"  "aaaaaaaaa" explicit

# --- Real-world names from the original report, not just synthetic runs. ---
run_case "real-nelisp-e"      "nelisp-e"      shorthand
run_case "real-nelisp-audit"  "audit--e"      shorthand
run_case "real-nelisp-dash-e" "nelisp--e"     shorthand
run_case "real-nelisp-long"   "nelisp-audit--e" shorthand

if [ "$smoke_findings" -ne 0 ]; then
    smoke_status=1
fi

echo "GATE-COUNT checked=$smoke_checked findings=$smoke_findings"
exit "$smoke_status"
