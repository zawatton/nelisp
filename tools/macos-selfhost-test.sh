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
# A no-dyld image is SIGKILLed by the kernel, so the writer emits the
# proven dyld+libSystem container that does its work via raw `svc'
# syscalls.  Zero Rust, zero external compiler/linker — only codesign.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$here"
EMACS="${EMACS:-emacs}"
fail=0

build_run() {            # NAME  PROGRAM-SEXP  EXPECTED-EXIT
  local name="$1" prog="$2" want="$3" out="/tmp/nelisp-macos-$1"
  rm -f "$out"
  if ! "$EMACS" --batch -Q -L lisp -L src -L scripts -l nelisp-macos-build \
        --eval "(nelisp-macos-build-program (quote $prog) \"$out\")" \
        >/dev/null 2>"/tmp/nelisp-macos-$1.log"; then
    echo "[macos] FAIL: $name — build error:"; sed 's/^/    /' "/tmp/nelisp-macos-$1.log" | tail -4
    fail=1; return
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

if [ "$fail" = 0 ]; then
  echo "[macos] all PASS — pure-elisp aarch64 -> native macOS arm64 self-host smoke OK"
  exit 0
else
  exit 1
fi
