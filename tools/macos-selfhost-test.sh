#!/usr/bin/env bash
# macOS arm64 (Apple Silicon) self-host smoke test.
#
# Builds small programs through the pure-elisp Phase-47 aarch64 backend
# -> Mach-O MH_EXECUTE (nelisp-mach-o-write-executable), ad-hoc signs
# them with the system codesign (Apple Silicon mandates a signature),
# runs each, and asserts its exit code.  Run on an M1/M2/... Mac with
# Emacs installed:
#
#     tools/macos-selfhost-test.sh
#
# On non-macOS hosts, validate that every smoke program still compiles to
# a Mach-O image without signing/running it:
#
#     tools/macos-selfhost-test.sh --emit-only
#
# A no-dyld image is SIGKILLed by the kernel, so the writer emits the
# proven dyld+libSystem container that does its work via raw `svc'
# syscalls.  Zero Rust, zero external compiler/linker — only codesign.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$here"
EMACS="${EMACS:-emacs}"
EMIT_ONLY="${EMIT_ONLY:-0}"
fail=0
SELECTED_SMOKES=()

usage() {
  echo "usage: $0 [--emit-only] [--smoke all|NAME] [--emacs EMACS] [--list]" >&2
}

SMOKE_NAMES=(
  exit42 loop fact alloc cons sexp let setq-local str ptr cas dealloc
  cons-set cond logic cons-clone boxed names call4-outs str-helpers lits
  extern aot-jump aot-roots f64-sexp callptr
)

smoke_exists() {
  local want="$1" item
  for item in "${SMOKE_NAMES[@]}"; do
    if [ "$item" = "$want" ]; then
      return 0
    fi
  done
  return 1
}

while [ "$#" -gt 0 ]; do
  case "$1" in
    --emacs)
      if [ "$#" -lt 2 ]; then usage; exit 2; fi
      EMACS="$2"
      shift 2
      ;;
    --emit-only)
      EMIT_ONLY=1
      shift
      ;;
    --smoke)
      if [ "$#" -lt 2 ]; then usage; exit 2; fi
      if [ "$2" = "all" ]; then
        SELECTED_SMOKES=()
        shift 2
        continue
      fi
      if ! smoke_exists "$2"; then
        echo "[macos] FAIL: unknown smoke '$2'" >&2
        echo "available smokes: all ${SMOKE_NAMES[*]}" >&2
        exit 2
      fi
      SELECTED_SMOKES+=("$2")
      shift 2
      ;;
    --list)
      echo "available smokes:"
      echo "  all"
      for name in "${SMOKE_NAMES[@]}"; do
        echo "  $name"
      done
      exit 0
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      usage
      exit 2
      ;;
  esac
done

selected_smoke_p() {
  local name="$1" item
  if [ "${#SELECTED_SMOKES[@]}" -eq 0 ]; then
    return 0
  fi
  for item in "${SELECTED_SMOKES[@]}"; do
    if [ "$item" = "$name" ]; then
      return 0
    fi
  done
  return 1
}

build_run() {            # NAME  PROGRAM-SEXP  EXPECTED-EXIT
  local name="$1" prog="$2" want="$3" out="/tmp/nelisp-macos-$1"
  if ! selected_smoke_p "$name"; then
    return
  fi
  rm -f "$out"
  if ! "$EMACS" --batch -Q -L lisp -L src -L scripts -l nelisp-macos-build \
        --eval "(nelisp-macos-build-program (quote $prog) \"$out\")" \
        >/dev/null 2>"/tmp/nelisp-macos-$1.log"; then
    echo "[macos] FAIL: $name — build error:"; sed 's/^/    /' "/tmp/nelisp-macos-$1.log" | tail -4
    fail=1; return
  fi
  if [ "$EMIT_ONLY" = 1 ]; then
    echo "[macos] PASS: $name -> built"
    return
  fi
  codesign -f -s - "$out" >/dev/null 2>&1 || { echo "[macos] FAIL: $name — codesign"; fail=1; return; }
  chmod +x "$out"
  set +e; "$out"; local got=$?; set -e
  if [ "$got" = "$want" ]; then
    echo "[macos] PASS: $name -> exit $got"
  else
    echo "[macos] FAIL: $name -> exit $got (expected $want)"; fail=1
  fi
}

# exit(42) via a raw Darwin exit syscall (SVC #0x80).
build_run exit42 '(syscall-direct 1 42 0 0 0 0 0)' 42

# while-loop summing 0..9 in mmap'd memory (control flow + ptr + arith).
build_run loop '(seq
  (syscall-direct 197 8589934592 16384 3 4114 -1 0)
  (ptr-write-u64 8589934592 0 0)
  (ptr-write-u64 8589934592 8 0)
  (while (< (ptr-read-u64 8589934592 0) 10)
    (seq
      (ptr-write-u64 8589934592 8 (+ (ptr-read-u64 8589934592 8) (ptr-read-u64 8589934592 0)))
      (ptr-write-u64 8589934592 0 (+ (ptr-read-u64 8589934592 0) 1))))
  (syscall-direct 1 (ptr-read-u64 8589934592 8) 0 0 0 0 0))' 45

# recursive factorial: fact(5) = 120 (function calls + recursion + frame).
build_run fact '(seq
  (defun fact (n) (if (< n 1) 1 (* n (fact (- n 1)))))
  (exit (fact 5)))' 120

# arena allocator: nl_alloc_bytes (atomic-bump) + the `alloc-bytes` op.
# mmap a 1 MiB arena at 8 GiB (above __PAGEZERO); reserve arena[0] as the
# bump pointer (init = arena+16); alloc 16 bytes (-> arena+16), store 99,
# read it back.  Exercises alloc-bytes -> BL nl_alloc_bytes end to end.
build_run alloc '(seq
  (defun nl_alloc_bytes (size align) (atomic-fetch-add 8589934592 size))
  (defun run ()
    (seq
      (syscall-direct 197 8589934592 1048576 3 4114 -1 0)
      (ptr-write-u64 8589934592 0 (+ 8589934592 16))
      (ptr-write-u64 (alloc-bytes 16 8) 0 99)
      (ptr-read-u64 8589934592 16)))
  (exit (run)))' 99

# cons round-trip: build a cons cell whose car is Int(7), read the car
# back -> 7.  Exercises sexp-int-make, cons-make (nl_alloc_consbox + box
# copies), cons-car (boxed-slot copy), and the Sexp 32-byte layout.
# Manual 32-byte slots live in [arena+64 .. arena+512); the bump
# allocator hands out boxes from arena+512 on.
build_run cons '(seq
  (defun nl_alloc_bytes (size align) (atomic-fetch-add 8589934592 size))
  (defun nl_alloc_consbox () (nl_alloc_bytes 72 8))
  (defun run ()
    (seq
      (syscall-direct 197 8589934592 1048576 3 4114 -1 0)
      (ptr-write-u64 8589934592 0 8589935104)
      (sexp-int-make 8589934656 7)
      (sexp-int-make 8589934720 0)
      (cons-make 8589934656 8589934720 8589934784)
      (cons-car 8589934784 8589934848)
      (ptr-read-u64 8589934848 8)))
  (exit (run)))' 7

# sexp readers: make Int(42), then read its tag (= 2 = SEXP_TAG_INT) and
# its payload (= 42) -> 2*100 + 42 = 242.  Exercises sexp-tag (LDRB) and
# sexp-int-unwrap (LDR payload).
build_run sexp '(seq
  (defun run ()
    (seq
      (syscall-direct 197 8589934592 1048576 3 4114 -1 0)
      (sexp-int-make 8589934656 42)
      (+ (* (sexp-tag 8589934656) 100) (sexp-int-unwrap 8589934656))))
  (exit (run)))' 242

# local variables: f(x) = let y = x+1 in y+y -> f(5) = 12.  Exercises
# let-rt (frame-slot reservation in the prologue + STUR spill + LDUR
# reload), which the reader relies on for parser state.
build_run let '(seq
  (defun f (x) (let ((y (+ x 1))) (+ y y)))
  (exit (f 5)))' 12

# local setq: update a runtime-let GP frame slot and read it back.
# This exercises the aarch64 `setq-local' frame-store path.
build_run setq-local '(seq
  (defun bump (x)
    (let ((i 0))
      (seq
        (setq i (+ i x))
        i)))
  (exit (bump 15)))' 15

# string readers: hand-build a Sexp::Str over a "Hi" buffer (tag@0=5,
# ptr@16=buf, length@24=2), then str-len (= 2) + str-byte-at index 1
# (= 'i' = 105) -> 107.  These are the reader's input-scanning ops.
build_run str '(seq
  (defun run ()
    (seq
      (syscall-direct 197 8589934592 1048576 3 4114 -1 0)
      (ptr-write-u64 8589934592 256 26952)
      (ptr-write-u64 8589934656 0 5)
      (ptr-write-u64 8589934656 16 8589934848)
      (ptr-write-u64 8589934656 24 2)
      (+ (str-len 8589934656) (str-byte-at 8589934656 1))))
  (exit (run)))' 107

# width-specific pointer load/store: write a u32 (= 0x030201, LE bytes
# [01 02 03 00]) then pick its bytes back with u8 reads (1,2,3); plus a
# u16 round-trip (40), a u8 round-trip (5) and a u32 round-trip (10).
# 1 + 2*2 + 4*3 + 40 + 5 + 10 = 72.  Exercises ptr-{read,write}-u{8,16,32}.
build_run ptr '(seq
  (defun run ()
    (seq
      (syscall-direct 197 8589934592 1048576 3 4114 -1 0)
      (ptr-write-u32 8589934592 256 197121)
      (ptr-write-u16 8589934592 260 40)
      (ptr-write-u8 8589934592 262 5)
      (ptr-write-u32 8589934592 264 10)
      (+ (ptr-read-u8 8589934592 256)
         (+ (* 2 (ptr-read-u8 8589934592 257))
            (+ (* 4 (ptr-read-u8 8589934592 258))
               (+ (ptr-read-u16 8589934592 260)
                  (+ (ptr-read-u8 8589934592 262)
                     (ptr-read-u32 8589934592 264))))))))
  (exit (run)))' 72

# compare-exchange: first CAS fails (7 != expected 8, no write), second
# succeeds (7 -> 42).  Result = fail*10 + success*20 + final-value = 62.
build_run cas '(seq
  (defun run ()
    (seq
      (syscall-direct 197 8589934592 1048576 3 4114 -1 0)
      (ptr-write-u64 8589934592 0 7)
      (ptr-write-u64 8589935104 0 (atomic-compare-exchange 8589934592 8 99))
      (ptr-write-u64 8589935104 8 (atomic-compare-exchange 8589934592 7 42))
      (+ (* 10 (ptr-read-u64 8589935104 0))
         (+ (* 20 (ptr-read-u64 8589935104 8))
            (ptr-read-u64 8589934592 0)))))
  (exit (run)))' 62

# dealloc-bytes: alloc 16 bytes then free them; dealloc returns the 1
# sentinel (the bump arena makes free a no-op) -> 1 + 41 = 42.
build_run dealloc '(seq
  (defun nl_alloc_bytes (size align) (atomic-fetch-add 8589934592 size))
  (defun nl_dealloc_bytes (ptr size align) 0)
  (defun run ()
    (seq
      (syscall-direct 197 8589934592 1048576 3 4114 -1 0)
      (ptr-write-u64 8589934592 0 (+ 8589934592 16))
      (+ (dealloc-bytes (alloc-bytes 16 8) 16 8) 41)))
  (exit (run)))' 42

# mutable cons: build (1 . 2), then overwrite car := Int(30) via
# cons-set-car and cdr := Int(4) via cons-set-cdr, read both back ->
# 30 + 4 = 34.  Manual 32-byte Sexp slots live at arena+64..+448; the
# bump allocator (cons-make's box) starts at arena+512.
build_run cons-set '(seq
  (defun nl_alloc_bytes (size align) (atomic-fetch-add 8589934592 size))
  (defun nl_alloc_consbox () (nl_alloc_bytes 72 8))
  (defun nl_consbox_set_car (box valptr)
    (seq
      (ptr-write-u64 box 0 (ptr-read-u64 valptr 0))
      (ptr-write-u64 box 8 (ptr-read-u64 valptr 8))
      (ptr-write-u64 box 16 (ptr-read-u64 valptr 16))
      (ptr-write-u64 box 24 (ptr-read-u64 valptr 24))))
  (defun nl_consbox_set_cdr (box valptr)
    (seq
      (ptr-write-u64 box 32 (ptr-read-u64 valptr 0))
      (ptr-write-u64 box 40 (ptr-read-u64 valptr 8))
      (ptr-write-u64 box 48 (ptr-read-u64 valptr 16))
      (ptr-write-u64 box 56 (ptr-read-u64 valptr 24))))
  (defun run ()
    (seq
      (syscall-direct 197 8589934592 1048576 3 4114 -1 0)
      (ptr-write-u64 8589934592 0 8589935104)
      (sexp-int-make 8589934656 1)
      (sexp-int-make 8589934720 2)
      (cons-make 8589934656 8589934720 8589934784)
      (sexp-int-make 8589934848 30)
      (sexp-int-make 8589934912 4)
      (cons-set-car 8589934784 8589934848)
      (cons-set-cdr 8589934784 8589934912)
      (cons-car 8589934784 8589934976)
      (cons-cdr 8589934784 8589935040)
      (+ (ptr-read-u64 8589934976 8) (ptr-read-u64 8589935040 8))))
  (exit (run)))' 34

# cond first-match dispatch: classify(2) hits the 2nd clause -> 50.
build_run cond '(seq
  (defun classify (x)
    (cond ((= x 1) 100) ((= x 2) 50) (t 7)))
  (exit (classify 2)))' 50

# and/or short-circuit: (and 1 1) -> 1 -> 10 ; (or 0 1) -> 1 -> 5 ; = 15.
build_run logic '(seq
  (defun run () (+ (if (and 1 1) 10 0) (if (or 0 1) 5 0)))
  (exit (run)))' 15

# cons-make-with-clone: fused (alloc box + deep-clone car/cdr).  Clone
# Int(20) into car and Int(3) into cdr, read both back -> 20 + 3 = 23.
# nl_sexp_clone_into is stubbed here as a 32-byte copy (the real one is
# refcount/String aware); box->car @0, box->cdr @offset-cdr(32).
build_run cons-clone '(seq
  (defun nl_alloc_bytes (size align) (atomic-fetch-add 8589934592 size))
  (defun nl_alloc_consbox () (nl_alloc_bytes 72 8))
  (defun nl_sexp_clone_into (src dst)
    (seq
      (ptr-write-u64 dst 0 (ptr-read-u64 src 0))
      (ptr-write-u64 dst 8 (ptr-read-u64 src 8))
      (ptr-write-u64 dst 16 (ptr-read-u64 src 16))
      (ptr-write-u64 dst 24 (ptr-read-u64 src 24))))
  (defun run ()
    (seq
      (syscall-direct 197 8589934592 1048576 3 4114 -1 0)
      (ptr-write-u64 8589934592 0 8589935104)
      (sexp-int-make 8589934656 20)
      (sexp-int-make 8589934720 3)
      (cons-make-with-clone 8589934656 8589934720 8589934784)
      (cons-car 8589934784 8589934976)
      (cons-cdr 8589934784 8589935040)
      (+ (ptr-read-u64 8589934976 8) (ptr-read-u64 8589935040 8))))
  (exit (run)))' 23

# boxed data ops: cell-make/value/set/null, vector make/ref/ref-ptr/set,
# record make/type-tag/slot-ref/slot-ref-ptr/set/count, and cons-cdr-raw.
# The local helpers implement the minimal Vec/NlCell/NlRecord layouts the
# aarch64 emitters read: NlVector data@+8 len@+16; NlRecord data@+40 len@+48.
build_run boxed '(seq
  (defun nl_alloc_bytes (size align) (atomic-fetch-add 8589934592 size))
  (defun nl_sexp_clone_into (src dst)
    (seq
      (ptr-write-u64 dst 0 (ptr-read-u64 src 0))
      (ptr-write-u64 dst 8 (ptr-read-u64 src 8))
      (ptr-write-u64 dst 16 (ptr-read-u64 src 16))
      (ptr-write-u64 dst 24 (ptr-read-u64 src 24))))
  (defun nl_alloc_consbox () (nl_alloc_bytes 72 8))
  (defun nl_alloc_cell (valptr)
    (let ((box (nl_alloc_bytes 40 8)))
      (seq (nl_sexp_clone_into valptr box) box)))
  (defun nl_cell_set_value (box valptr)
    (nl_sexp_clone_into valptr box))
  (defun nl_alloc_vector (cap)
    (let ((box (nl_alloc_bytes 32 8)))
      (seq
        (ptr-write-u64 box 8 (nl_alloc_bytes (* cap 32) 8))
        (ptr-write-u64 box 16 cap)
        box)))
  (defun nl_vector_set_slot (vec idx valptr)
    (nl_sexp_clone_into valptr (+ (ptr-read-u64 vec 8) (* idx 32))))
  (defun nl_alloc_record (tagptr count)
    (let ((box (nl_alloc_bytes 64 8)))
      (seq
        (nl_sexp_clone_into tagptr box)
        (ptr-write-u64 box 40 (nl_alloc_bytes (* count 32) 8))
        (ptr-write-u64 box 48 count)
        box)))
  (defun nl_record_set_slot (rec idx valptr)
    (nl_sexp_clone_into valptr (+ (ptr-read-u64 rec 40) (* idx 32))))
  (defun run ()
    (seq
      (syscall-direct 197 8589934592 1048576 3 4114 -1 0)
      (ptr-write-u64 8589934592 0 8589938688)
      (sexp-int-make 8589934656 5)
      (sexp-int-make 8589934720 17)
      (sexp-int-make 8589934784 19)
      (cell-make 8589934720 8589934848)
      (cell-value 8589934848 8589934912)
      (cell-set-value 8589934848 8589934784)
      (cell-value 8589934848 8589934976)
      (vector-make 2 8589935040)
      (vector-slot-set 8589935040 0 8589934720)
      (vector-slot-set 8589935040 1 8589934784)
      (vector-ref 8589935040 1 8589935104)
      (record-make 8589934656 2 8589935168)
      (record-slot-set 8589935168 0 8589934720)
      (record-slot-set 8589935168 1 8589934784)
      (record-slot-ref 8589935168 0 8589935232)
      (record-type-tag 8589935168 8589935296)
      (cons-make 8589934720 8589934784 8589935360)
      (+ (+ (ptr-read-u64 8589934912 8)
            (ptr-read-u64 8589934976 8))
         (+ (vector-len 8589935040)
            (+ (sexp-int-unwrap (vector-ref-ptr 8589935040 0))
               (+ (ptr-read-u64 8589935104 8)
                  (+ (record-slot-count 8589935168)
                     (+ (sexp-int-unwrap (record-slot-ref-ptr 8589935168 1))
                        (+ (ptr-read-u64 8589935232 8)
                           (+ (ptr-read-u64 8589935296 8)
                              (+ (if (< 0 (sexp-payload-ptr-record 8589935168)) 1 0)
                                 (if (= (cons-cdr-raw 8589935360) 0) 3 0))))))))))))
      (exit (run)))' 121

# string/symbol name ops: hand-build Symbol("ab") and Str("ab"), check
# str-bytes, str-bytes-ptr, str-eq, symbol-eq, symbol-name-eq,
# sexp-name-eq, and sexp-write-nil/t.  Expected:
# 97 + 3 + 5 + 7 + 11 + 0 + 17 + 19 = 159.
build_run names '(seq
  (defun nl_str_bytes_ptr (ptr) (str-bytes ptr))
  (defun run ()
    (seq
      (syscall-direct 197 8589934592 1048576 3 4114 -1 0)
      (ptr-write-u64 8589934848 0 25185)
      (ptr-write-u64 8589934656 0 4)
      (ptr-write-u64 8589934656 16 8589934848)
      (ptr-write-u64 8589934656 24 2)
      (ptr-write-u64 8589934720 0 4)
      (ptr-write-u64 8589934720 16 8589934848)
      (ptr-write-u64 8589934720 24 2)
      (ptr-write-u64 8589934784 0 5)
      (ptr-write-u64 8589934784 16 8589934848)
      (ptr-write-u64 8589934784 24 2)
      (sexp-write-nil 8589934912)
      (sexp-write-t 8589934976)
      (+ (ptr-read-u8 (str-bytes 8589934784) 0)
         (+ (* 3 (str-eq 8589934784 8589934784))
            (+ (* 5 (symbol-eq 8589934656 8589934720))
               (+ (* 7 (symbol-name-eq 8589934656 "ab"))
                  (+ (* 11 (sexp-name-eq 8589934784 "ab"))
                     (+ (* 13 (sexp-tag 8589934912))
                        (+ (* 17 (sexp-tag 8589934976))
                           (* 19 (if (= (str-bytes-ptr 8589934784)
                                        (str-bytes 8589934784)) 1 0)))))))))))
  (exit (run)))' 159

# Four-arg local call writing through the 3rd/4th arguments.  This
# isolates the same out-slot pattern used by str-codepoint-at without
# going through that builtin's dedicated emitter.
build_run call4-outs '(seq
  (defun write_outs (a b cp-slot width-slot)
    (seq
      (ptr-write-u64 cp-slot 0 97)
      (ptr-write-u64 width-slot 0 1)
      1))
  (defun run ()
    (seq
      (syscall-direct 197 8589934592 1048576 3 4114 -1 0)
      (write_outs 11 22 8589934976 8589935040)
      (+ 1
         (+ (ptr-read-u64 8589934976 0)
            (ptr-read-u64 8589935040 0)))))
  (exit (run)))' 99

# string writers + mut-str + UTF-8 helper call shuffles.  Helpers are
# stubbed locally; this checks aarch64 BL arg order and return handling.
# The mut-str push ops return a void-helper sentinel 1 after BL.
# Expected = 5+4+6+1+1+2+5+2+1+97+1+1 = 126.
build_run str-helpers '(seq
  (defun nl_alloc_str (bytes len slot)
    (seq
      (ptr-write-u64 slot 0 5)
      (ptr-write-u64 slot 16 bytes)
      (ptr-write-u64 slot 24 len)
      slot))
  (defun nl_alloc_symbol (bytes len slot)
    (seq
      (ptr-write-u64 slot 0 4)
      (ptr-write-u64 slot 16 bytes)
      (ptr-write-u64 slot 24 len)
      slot))
  (defun nl_alloc_mut_str (cap slot)
    (seq (ptr-write-u64 slot 0 6) (ptr-write-u64 slot 8 cap) slot))
  (defun nl_mut_str_push_byte (ptr byte) 0)
  (defun nl_mut_str_push_codepoint (ptr cp) 0)
  (defun nl_mut_str_len (ptr) 2)
  (defun nl_mut_str_finalize (ptr slot)
    (seq (ptr-write-u64 slot 0 5) slot))
  (defun nl_str_char_count (ptr) (ptr-read-u64 ptr 24))
  (defun nl_str_codepoint_at (ptr idx cp-slot width-slot)
    (seq
      (ptr-write-u64 cp-slot 0 97)
      (ptr-write-u64 width-slot 0 1)
      1))
  (defun nl_str_is_alphanumeric_at (ptr idx) 1)
  (defun run ()
    (seq
      (syscall-direct 197 8589934592 1048576 3 4114 -1 0)
      (ptr-write-u64 8589934848 0 25185)
      (sexp-write-str 8589934656 8589934848 2)
      (sexp-write-symbol 8589934720 8589934848 2)
      (mut-str-make-empty 8589934784 3)
      (str-codepoint-at 8589934656 0 8589934976 8589935040)
      (+ (sexp-tag 8589934656)
         (+ (sexp-tag 8589934720)
            (+ (sexp-tag 8589934784)
               (+ (mut-str-push-byte 8589934784 97)
                  (+ (mut-str-push-codepoint 8589934784 98)
                     (+ (mut-str-len 8589934784)
                        (+ (sexp-tag (mut-str-finalize 8589934784 8589934912))
                           (+ (str-char-count 8589934656)
                              (+ 1
                                 (+ (ptr-read-u64 8589934976 0)
                                    (+ (ptr-read-u64 8589935040 0)
                                       (str-is-alphanumeric-at 8589934656 0))))))))))))))
  (exit (run)))' 126

# literal writers: aarch64 builds a temporary stack byte buffer and calls
# nl_alloc_symbol / nl_alloc_str.  Stub helpers record the first byte.
# Expected = Symbol tag 4 + 'a' 97 + Str tag 5 + 'a' 97 = 203.
build_run lits '(seq
  (defun nl_alloc_symbol (bytes len slot)
    (seq
      (ptr-write-u64 slot 0 4)
      (ptr-write-u64 slot 8 (ptr-read-u8 bytes 0))
      (ptr-write-u64 slot 24 len)
      slot))
  (defun nl_alloc_str (bytes len slot)
    (seq
      (ptr-write-u64 slot 0 5)
      (ptr-write-u64 slot 8 (ptr-read-u8 bytes 0))
      (ptr-write-u64 slot 24 len)
      slot))
  (defun run ()
    (seq
      (syscall-direct 197 8589934592 1048576 3 4114 -1 0)
      (sexp-write-symbol-lit 8589934656 "ab")
      (sexp-write-str-lit 8589934720 "ab")
      (+ (sexp-tag 8589934656)
         (+ (ptr-read-u64 8589934656 8)
            (+ (sexp-tag 8589934720)
               (ptr-read-u64 8589934720 8))))))
  (exit (run)))' 203

# extern-call: direct GP-only aarch64 BL path with six register args.
build_run extern '(seq
  (defun add6 (a b c d e f)
    (+ (+ (+ a b) (+ c d)) (+ e f)))
  (defun run () (extern-call add6 1 2 3 4 5 6))
  (exit (run)))' 21

# AOT machine landing jump: restore the current SP and branch to the
# landing label, skipping the sentinel value.
build_run aot-jump '(seq
  (defun run (value)
    (seq
      (aot-machine-landing-jump (aot-current-sp) doc129_landing_pad)
      99
      (aot-landing-label doc129_landing_pad value)))
  (exit (run 37)))' 37

# AOT root scope: sexp-write-str auto-wraps with materialize/push/pop roots.
# Stub bridge helpers preserve the roots vector and writer returns tag 5.
build_run aot-roots '(seq
  (defun nelisp_aot_materialize_roots (mirror frames roots out scratch) roots)
  (defun nelisp_aot_push_roots (mirror frames roots out scratch) 0)
  (defun nelisp_aot_pop_roots (mirror frames roots out scratch) 0)
  (defun nl_alloc_str (bytes len slot)
    (seq
      (ptr-write-u64 slot 0 5)
      slot))
  (defun make-str
      ((out :type sexp)
       (mirror :type sexp)
       (frames :type sexp)
       (scratch :type sexp)
       (roots :type sexp)
       bytes)
    (sexp-write-str out bytes 2))
  (defun run ()
    (seq
      (syscall-direct 197 8589934592 1048576 3 4114 -1 0)
      (make-str 8589934656 1 2 3 4 8589934848)
      (sexp-tag 8589934656)))
  (exit (run)))' 5

# f64 Sexp ops: write 42.0 through the helper, unwrap raw bits, bitcast
# back to f64, truncate, and add the Float tag (3) -> 45.
build_run f64-sexp '(seq
  (defun nl_sexp_write_float (slot val)
    (seq
      (ptr-write-u64 slot 0 3)
      (ptr-write-u64 slot 8 4631107791820423168)
      slot))
  (defun run ()
    (seq
      (syscall-direct 197 8589934592 1048576 3 4114 -1 0)
      (sexp-write-float 8589934656 (i64-to-f64 42))
      (+ (sexp-tag 8589934656)
         (f64-to-i64-trunc
           (bits-to-f64
             (sexp-float-unwrap 8589934656))))))
  (exit (run)))' 45

# function pointers (Doc 133 P0): take add2's address (ADR), call it
# indirectly (BLR) with args 30, 12 -> 42.  Exercises addr-of + call-ptr,
# the keystone for the real combiner/eval dispatch on arm64.
build_run callptr '(seq
  (defun add2 (a b) (+ a b))
  (defun run () (call-ptr (addr-of add2) 30 12))
  (exit (run)))' 42

if [ "$fail" = 0 ]; then
  if [ "$EMIT_ONLY" = 1 ]; then
    echo "[macos] all PASS — pure-elisp aarch64 -> Mach-O emit-only smoke OK"
  else
    echo "[macos] all PASS — pure-elisp aarch64 -> native macOS arm64 self-host smoke OK"
  fi
  exit 0
else
  exit 1
fi
