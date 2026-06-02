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

if [ "$fail" = 0 ]; then
  echo "[macos] all PASS — pure-elisp aarch64 -> native macOS arm64 self-host smoke OK"
  exit 0
else
  exit 1
fi
