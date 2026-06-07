#!/usr/bin/env bash
# macOS arm64 (Apple Silicon) self-host smoke test.
#
# Builds small programs through the pure-elisp AOT aarch64 backend
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
# To isolate artifacts for parallel runs or repeated local experiments:
#
#     tools/macos-selfhost-test.sh --emit-only --out-dir target/macos-smoke-run1
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
OUT_DIR="${NELISP_MACOS_SMOKE_OUT_DIR:-$here/target/macos-smoke}"

usage() {
  echo "usage: $0 [--emit-only] [--smoke all|NAME] [--emacs EMACS] [--out-dir DIR] [--list]" >&2
}

SMOKE_NAMES=(
  exit42 loop fact alloc mprotect-munmap cons sexp let setq-local str ptr
  cas dealloc cons-set cond logic write-stdout read-stdin pipe getpid
  fork-wait fork-execve createfile-write lseek-fstat file-mmap socket-close
  dup-fcntl cons-clone boxed names call4-outs str-helpers lits extern
  aot-jump aot-roots f64-sexp callptr
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
    --out-dir)
      if [ "$#" -lt 2 ]; then usage; exit 2; fi
      OUT_DIR="$2"
      shift 2
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

mkdir -p "$OUT_DIR"

echo "--- macOS arm64 Mach-O self-host smoke ---"
uname -a
"$EMACS" --version | head -1
echo "output: $OUT_DIR"

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

build_run() {            # NAME  PROGRAM-SEXP  EXPECTED-EXIT [EXPECTED-STDOUT] [STDIN-TEXT]
  local name="$1" prog="$2" want="$3" out="$OUT_DIR/nelisp-macos-$1"
  local log="$OUT_DIR/nelisp-macos-$1.build.log"
  local want_stdout="${4-}" stdin_text="${5-}" output got stdin_file
  if ! selected_smoke_p "$name"; then
    return
  fi
  rm -f "$out" "$log"
  if ! "$EMACS" --batch -Q -L lisp -L src -L scripts -l nelisp-macos-build \
        --eval "(nelisp-macos-build-program (quote $prog) \"$out\")" \
        >"$log" 2>&1; then
    echo "[macos] FAIL: $name — build error:"; sed 's/^/    /' "$log" | tail -4
    fail=1; return
  fi
  if [ "$EMIT_ONLY" = 1 ]; then
    echo "[macos] PASS: $name -> built"
    return
  fi
  codesign -f -s - "$out" >/dev/null 2>&1 || { echo "[macos] FAIL: $name — codesign"; fail=1; return; }
  chmod +x "$out"
  set +e
  if [ -n "$stdin_text" ]; then
    stdin_file="$OUT_DIR/nelisp-macos-$name.stdin"
    printf '%s' "$stdin_text" >"$stdin_file"
    output="$("$out" <"$stdin_file")"
    got=$?
  else
    output="$("$out")"
    got=$?
  fi
  set -e
  if [ "$got" = "$want" ]; then
    if [ -n "$want_stdout" ] && [ "$output" != "$want_stdout" ]; then
      echo "[macos] FAIL: $name -> stdout mismatch"
      printf '    expected: %s\n    actual  : %s\n' "$want_stdout" "$output"
      fail=1
    else
      echo "[macos] PASS: $name -> exit $got"
    fi
  else
    echo "[macos] FAIL: $name -> exit $got (expected $want)"; fail=1
  fi
}

# exit(42) via a raw Darwin exit syscall (SVC #0x80).
build_run exit42 '(syscall-direct 1 42 0 0 0 0 0)' 42

# while-loop summing 0..9 in mmap'd memory (control flow + ptr + arith).
build_run loop '(seq
  (syscall-direct 197 34359738368 16384 3 4114 -1 0)
  (ptr-write-u64 34359738368 0 0)
  (ptr-write-u64 34359738368 8 0)
  (while (< (ptr-read-u64 34359738368 0) 10)
    (seq
      (ptr-write-u64 34359738368 8 (+ (ptr-read-u64 34359738368 8) (ptr-read-u64 34359738368 0)))
      (ptr-write-u64 34359738368 0 (+ (ptr-read-u64 34359738368 0) 1))))
  (syscall-direct 1 (ptr-read-u64 34359738368 8) 0 0 0 0 0))' 45

# recursive factorial: fact(5) = 120 (function calls + recursion + frame).
build_run fact '(seq
  (defun fact (n) (if (< n 1) 1 (* n (fact (- n 1)))))
  (exit (fact 5)))' 120

# arena allocator: nl_alloc_bytes (atomic-bump) + the `alloc-bytes` op.
# mmap a 1 MiB arena at 32 GiB (above __PAGEZERO/dyld ranges); reserve arena[0] as the
# bump pointer (init = arena+16); alloc 16 bytes (-> arena+16), store 99,
# read it back.  Exercises alloc-bytes -> BL nl_alloc_bytes end to end.
build_run alloc '(seq
  (defun nl_alloc_bytes (size align) (atomic-fetch-add 34359738368 size))
  (defun run ()
    (seq
      (syscall-direct 197 34359738368 1048576 3 4114 -1 0)
      (ptr-write-u64 34359738368 0 (+ 34359738368 16))
      (ptr-write-u64 (alloc-bytes 16 8) 0 99)
      (ptr-read-u64 34359738368 16)))
  (exit (run)))' 99

# raw Darwin mmap(2) -> mprotect(2) -> munmap(2).  Windows has a
# VirtualProtect/VirtualFree smoke; this locks the corresponding Mach-O path.
build_run mprotect-munmap '(seq
  (defun run ()
    (let ((p (syscall-direct 197 34359738368 4096 3 4114 -1 0)))
      (if (= p 34359738368)
          (let ((mp (syscall-direct 74 34359738368 4096 1 0 0 0)))
            (let ((mu (syscall-direct 73 34359738368 4096 0 0 0 0)))
              (if (= mp 0)
                  (if (= mu 0) 42 13)
                13)))
        13)))
  (exit (run)))' 42

# cons round-trip: build a cons cell whose car is Int(7), read the car
# back -> 7.  Exercises sexp-int-make, cons-make (nl_alloc_consbox + box
# copies), cons-car (boxed-slot copy), and the Sexp 32-byte layout.
# Manual 32-byte slots live in [arena+64 .. arena+512); the bump
# allocator hands out boxes from arena+512 on.
build_run cons '(seq
  (defun nl_alloc_bytes (size align) (atomic-fetch-add 34359738368 size))
  (defun nl_alloc_consbox () (nl_alloc_bytes 24 8))
  (defun nl_sexp_clone_into (src dst)
    (seq (ptr-write-u64 dst 0 (ptr-read-u64 src 0))
         (ptr-write-u64 dst 8 (ptr-read-u64 src 8))
         (ptr-write-u64 dst 16 (ptr-read-u64 src 16))
         (ptr-write-u64 dst 24 (ptr-read-u64 src 24))))
  (defun nl_val_clone_into (src dst)
    (if (= (logand src 1) 1)
        (ptr-write-u64 dst 0 src)
      (let ((box (nl_alloc_bytes 32 8)))
        (seq (nl_sexp_clone_into src box)
             (ptr-write-u64 dst 0 box)))))
  (defun nl_val_load (word scratch)
    (if (= (logand word 1) 0) word
      (seq (ptr-write-u64 scratch 0 word) scratch)))
  (defun run ()
    (seq
      (syscall-direct 197 34359738368 1048576 3 4114 -1 0)
      (ptr-write-u64 34359738368 0 34359738880)
      (sexp-int-make 34359738432 7)
      (sexp-int-make 34359738496 0)
      (cons-make 34359738432 34359738496 34359738560)
      (cons-car 34359738560 34359738624)
      (ptr-read-u64 34359738624 8)))
  (exit (run)))' 7

# sexp readers: make Int(42), then read its tag (= 2 = SEXP_TAG_INT) and
# its payload (= 42) -> 2*100 + 42 = 242.  Exercises sexp-tag (LDRB) and
# sexp-int-unwrap (LDR payload).
build_run sexp '(seq
  (defun run ()
    (seq
      (syscall-direct 197 34359738368 1048576 3 4114 -1 0)
      (sexp-int-make 34359738432 42)
      (+ (* (sexp-tag 34359738432) 100) (sexp-int-unwrap 34359738432))))
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
      (syscall-direct 197 34359738368 1048576 3 4114 -1 0)
      (ptr-write-u64 34359738368 256 26952)
      (ptr-write-u64 34359738432 0 5)
      (ptr-write-u64 34359738432 16 34359738624)
      (ptr-write-u64 34359738432 24 2)
      (+ (str-len 34359738432) (str-byte-at 34359738432 1))))
  (exit (run)))' 107

# width-specific pointer load/store: write a u32 (= 0x030201, LE bytes
# [01 02 03 00]) then pick its bytes back with u8 reads (1,2,3); plus a
# u16 round-trip (40), a u8 round-trip (5) and a u32 round-trip (10).
# 1 + 2*2 + 4*3 + 40 + 5 + 10 = 72.  Exercises ptr-{read,write}-u{8,16,32}.
build_run ptr '(seq
  (defun run ()
    (seq
      (syscall-direct 197 34359738368 1048576 3 4114 -1 0)
      (ptr-write-u32 34359738368 256 197121)
      (ptr-write-u16 34359738368 260 40)
      (ptr-write-u8 34359738368 262 5)
      (ptr-write-u32 34359738368 264 10)
      (+ (ptr-read-u8 34359738368 256)
         (+ (* 2 (ptr-read-u8 34359738368 257))
            (+ (* 4 (ptr-read-u8 34359738368 258))
               (+ (ptr-read-u16 34359738368 260)
                  (+ (ptr-read-u8 34359738368 262)
                     (ptr-read-u32 34359738368 264))))))))
  (exit (run)))' 72

# compare-exchange: first CAS fails (7 != expected 8, no write), second
# succeeds (7 -> 42).  Result = fail*10 + success*20 + final-value = 62.
build_run cas '(seq
  (defun run ()
    (seq
      (syscall-direct 197 34359738368 1048576 3 4114 -1 0)
      (ptr-write-u64 34359738368 0 7)
      (ptr-write-u64 34359738880 0 (atomic-compare-exchange 34359738368 8 99))
      (ptr-write-u64 34359738880 8 (atomic-compare-exchange 34359738368 7 42))
      (+ (* 10 (ptr-read-u64 34359738880 0))
         (+ (* 20 (ptr-read-u64 34359738880 8))
            (ptr-read-u64 34359738368 0)))))
  (exit (run)))' 62

# dealloc-bytes: alloc 16 bytes then free them; dealloc returns the 1
# sentinel (the bump arena makes free a no-op) -> 1 + 41 = 42.
build_run dealloc '(seq
  (defun nl_alloc_bytes (size align) (atomic-fetch-add 34359738368 size))
  (defun nl_dealloc_bytes (ptr size align) 0)
  (defun run ()
    (seq
      (syscall-direct 197 34359738368 1048576 3 4114 -1 0)
      (ptr-write-u64 34359738368 0 (+ 34359738368 16))
      (+ (dealloc-bytes (alloc-bytes 16 8) 16 8) 41)))
  (exit (run)))' 42

# mutable cons: build (1 . 2), then overwrite car := Int(30) via
# cons-set-car and cdr := Int(4) via cons-set-cdr, read both back ->
# 30 + 4 = 34.  Manual 32-byte Sexp slots live at arena+64..+448; the
# bump allocator (cons-make's box) starts at arena+512.
build_run cons-set '(seq
  (defun nl_alloc_bytes (size align) (atomic-fetch-add 34359738368 size))
  (defun nl_alloc_consbox () (nl_alloc_bytes 24 8))
  (defun nl_sexp_clone_into (src dst)
    (seq (ptr-write-u64 dst 0 (ptr-read-u64 src 0))
         (ptr-write-u64 dst 8 (ptr-read-u64 src 8))
         (ptr-write-u64 dst 16 (ptr-read-u64 src 16))
         (ptr-write-u64 dst 24 (ptr-read-u64 src 24))))
  (defun nl_val_clone_into (src dst)
    (if (= (logand src 1) 1)
        (ptr-write-u64 dst 0 src)
      (let ((box (nl_alloc_bytes 32 8)))
        (seq (nl_sexp_clone_into src box)
             (ptr-write-u64 dst 0 box)))))
  (defun nl_val_load (word scratch)
    (if (= (logand word 1) 0) word
      (seq (ptr-write-u64 scratch 0 word) scratch)))
  (defun nl_consbox_set_car (box valptr) (nl_val_clone_into valptr box))
  (defun nl_consbox_set_cdr (box valptr) (nl_val_clone_into valptr (+ box 8)))
  (defun run ()
    (seq
      (syscall-direct 197 34359738368 1048576 3 4114 -1 0)
      (ptr-write-u64 34359738368 0 34359738880)
      (sexp-int-make 34359738432 1)
      (sexp-int-make 34359738496 2)
      (cons-make 34359738432 34359738496 34359738560)
      (sexp-int-make 34359738624 30)
      (sexp-int-make 34359738688 4)
      (cons-set-car 34359738560 34359738624)
      (cons-set-cdr 34359738560 34359738688)
      (cons-car 34359738560 34359738752)
      (cons-cdr 34359738560 34359738816)
      (+ (ptr-read-u64 34359738752 8) (ptr-read-u64 34359738816 8))))
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

# raw Darwin write(2) to stdout: verifies native Mach-O can write to fd 1.
build_run write-stdout '(seq
  (defun run ()
    (seq
      (syscall-direct 197 34359738368 1048576 3 4114 -1 0)
      (ptr-write-u64 34359738368 256 8243311830880773480)
      (ptr-write-u64 34359738368 264 8316297369811447151)
      (ptr-write-u64 34359738368 272 32492094948712560)
      (syscall-direct 4 1 34359738624 23 0 0 0)
      (syscall-direct 1 42 0 0 0 0 0)))
  (exit (run)))' 42 "hello from nelisp macos"

# raw Darwin read(2) from stdin: verifies native Mach-O can read fd 0.
build_run read-stdin '(seq
  (defun fail () (syscall-direct 1 13 0 0 0 0 0))
  (defun ok () (syscall-direct 1 42 0 0 0 0 0))
  (defun run ()
    (seq
      (syscall-direct 197 34359738368 1048576 3 4114 -1 0)
      (let ((n (syscall-direct 3 0 34359738624 17 0 0 0)))
        (if (= n 17)
            (if (= (ptr-read-u8 34359738368 256) 110)
                (if (= (ptr-read-u8 34359738368 263) 114)
                    (if (= (ptr-read-u8 34359738368 272) 101)
                        (ok)
                      (fail))
                  (fail))
              (fail))
          (fail)))))
  (exit (run)))' 42 "" "nelisp read smoke"

# Darwin fd-pair lifecycle: raw pipe(2) returns the fd pair in x0/x1.  Write
# 4 bytes through the x1 fd, read them back from the x0 fd, and close both ends.
# Mirrors the Windows CreatePipe self-host smoke at the descriptor/data-path
# level.
build_run pipe '(seq
  (defun fail () (syscall-direct 1 13 0 0 0 0 0))
  (defun ok () (syscall-direct 1 42 0 0 0 0 0))
  (defun good-fds (rfd wfd)
    (if (< 2 rfd)
        (if (< 2 wfd) 1 0)
      0))
  (defun run ()
    (seq
      (syscall-direct 197 34359738368 1048576 3 4114 -1 0)
      (let ((rfd (syscall-direct-store-x1 42 0 0 0 0 0 0 34359738368 256)))
        (let ((wfd (ptr-read-u64 34359738368 256)))
          (if (= (good-fds rfd wfd) 1)
              (seq
                (ptr-write-u32 34359738368 320 1701865840)
                (let ((w (syscall-direct 4 wfd 34359738688 4 0 0 0)))
                  (let ((n (syscall-direct 3 rfd 34359738752 4 0 0 0)))
                    (seq
                      (syscall-direct 6 rfd 0 0 0 0 0)
                      (syscall-direct 6 wfd 0 0 0 0 0)
                      (if (= w 4)
                          (if (= n 4)
                              (if (= (ptr-read-u32 34359738368 384) 1701865840)
                                  (ok)
                                (fail))
                            (fail))
                        (fail))))))
            (fail))))))
  (exit (run)))' 42

# raw Darwin getpid(2): verifies basic process syscall wiring.
build_run getpid '(seq
  (defun run ()
    (let ((pid (syscall-direct 20 0 0 0 0 0 0)))
      (if (< 0 pid) 42 13)))
  (exit (run)))' 42

# raw Darwin fork(2)+wait4(2): child exits 42, parent waits for the exact
# child pid and verifies the traditional wait status (42 << 8).  Darwin
# arm64 returns the fork parent/child discriminator in x1, so this uses the
# store-x1 syscall helper instead of assuming x0 alone is enough.
build_run fork-wait '(seq
  (defun fail (code) (syscall-direct 1 code 0 0 0 0 0))
  (defun ok () (syscall-direct 1 42 0 0 0 0 0))
  (defun wait-status-ok (status)
    (if (= status 10752)
        1
      (if (= status 42) 1 0)))
  (defun run ()
    (seq
      (syscall-direct 197 34359738368 1048576 3 4114 -1 0)
      (let ((pid (syscall-direct-store-x1 2 0 0 0 0 0 0 34359738368 240)))
        (let ((childp (ptr-read-u64 34359738368 240)))
          (if (or (= childp 1) (= pid 0))
              (syscall-direct 1 42 0 0 0 0 0)
            (if (< 0 pid)
                (let ((waited (syscall-direct 7 pid 34359738624 0 0 0 0)))
                  (if (= waited pid)
                      (if (= (wait-status-ok (ptr-read-u32 34359738368 256)) 1)
                          (ok)
                        (fail 33))
                    (fail 32)))
              (fail 31)))))))
  (exit (run)))' 42

# raw Darwin fork(2)+execve(2)+wait4(2): child execs `/bin/sh -c "exit 42"`,
# parent waits and verifies the status.  This mirrors Windows CreateProcess.
build_run fork-execve '(seq
  (defun fail () (syscall-direct 1 13 0 0 0 0 0))
  (defun ok () (syscall-direct 1 42 0 0 0 0 0))
  (defun run ()
    (seq
      (syscall-direct 197 34359738368 1048576 3 4114 -1 0)
      (ptr-write-u64 34359738368 256 29400045130965551)
      (ptr-write-u64 34359738368 272 25389)
      (ptr-write-u64 34359738368 288 14131062832199781)
      (ptr-write-u64 34359738368 512 34359738624)
      (ptr-write-u64 34359738368 520 34359738640)
      (ptr-write-u64 34359738368 528 34359738656)
      (ptr-write-u64 34359738368 536 0)
      (ptr-write-u64 34359738368 576 0)
      (let ((pid (syscall-direct-store-x1 2 0 0 0 0 0 0 34359738368 240)))
        (let ((childp (ptr-read-u64 34359738368 240)))
          (if (or (= childp 1) (= pid 0))
              (seq
                (syscall-direct 59 34359738624 34359738880 34359738944 0 0 0)
                (syscall-direct 1 13 0 0 0 0 0))
            (if (< 0 pid)
                (let ((waited (syscall-direct 7 pid 34359738688 0 0 0 0)))
                  (if (= waited pid)
                      (if (= (ptr-read-u32 34359738368 320) 10752)
                          (ok)
                        (fail))
                    (fail)))
              (fail)))))))
  (exit (run)))' 42

# raw Darwin open/write/close/unlink path: verifies simple file creation.
build_run createfile-write '(seq
  (defun fail () (syscall-direct 1 13 0 0 0 0 0))
  (defun ok () (syscall-direct 1 42 0 0 0 0 0))
  (defun run ()
    (seq
      (syscall-direct 197 34359738368 1048576 3 4114 -1 0)
      (ptr-write-u64 34359738368 256 7810770278772732975)
      (ptr-write-u64 34359738368 264 8026366082446029673)
      (ptr-write-u64 34359738368 272 111516299177331)
      (ptr-write-u64 34359738368 320 1819043144)
      (let ((fd (syscall-direct 5 34359738624 1537 420 0 0 0)))
        (if (< 2 fd)
            (let ((w (syscall-direct 4 fd 34359738688 4 0 0 0)))
              (seq
                (syscall-direct 6 fd 0 0 0 0 0)
                (syscall-direct 10 34359738624 0 0 0 0 0)
                (if (= w 4) (ok) (fail))))
          (fail)))))
  (exit (run)))' 42

# raw Darwin lseek(2)+fstat64(2): write/read through an O_RDWR file after
# seeking back to offset 0, then verify the descriptor accepts fstat.
build_run lseek-fstat '(seq
  (defun fail () (syscall-direct 1 13 0 0 0 0 0))
  (defun ok () (syscall-direct 1 42 0 0 0 0 0))
  (defun run ()
    (seq
      (syscall-direct 197 34359738368 1048576 3 4114 -1 0)
      (ptr-write-u64 34359738368 256 7810770278772732975)
      (ptr-write-u64 34359738368 264 8026366082446029673)
      (ptr-write-u64 34359738368 272 111516299177331)
      (ptr-write-u32 34359738368 320 1801807219)
      (let ((fd (syscall-direct 5 34359738624 1538 420 0 0 0)))
        (if (< 2 fd)
            (let ((w (syscall-direct 4 fd 34359738688 4 0 0 0)))
              (let ((pos (syscall-direct 199 fd 0 0 0 0 0)))
                (let ((n (syscall-direct 3 fd 34359738752 4 0 0 0)))
                  (let ((st (syscall-direct 189 fd 34359738816 0 0 0 0)))
                    (seq
                      (syscall-direct 6 fd 0 0 0 0 0)
                      (syscall-direct 10 34359738624 0 0 0 0 0)
                      (if (= w 4)
                          (if (= pos 0)
                              (if (= n 4)
                                  (if (= st 0)
                                      (if (= (ptr-read-u32 34359738368 384) 1801807219)
                                          (ok)
                                        (fail))
                                    (fail))
                                (fail))
                            (fail))
                        (fail)))))))
          (fail)))))
  (exit (run)))' 42

# raw Darwin file-backed mmap(2): create a file, map it read-only, verify the
# mapped bytes, then munmap/close/unlink.  Mirrors Windows filemapping smoke.
build_run file-mmap '(seq
  (defun fail () (syscall-direct 1 13 0 0 0 0 0))
  (defun ok () (syscall-direct 1 42 0 0 0 0 0))
  (defun good (w value mu)
    (if (= w 4)
        (if (= value 561013101)
            (if (= mu 0) 1 0)
          0)
      0))
  (defun run ()
    (seq
      (syscall-direct 197 34359738368 1048576 3 4114 -1 0)
      (ptr-write-u64 34359738368 256 7810770278772732975)
      (ptr-write-u64 34359738368 264 8026366082446029673)
      (ptr-write-u64 34359738368 272 111516299177331)
      (ptr-write-u32 34359738368 320 561013101)
      (let ((fd (syscall-direct 5 34359738624 1538 420 0 0 0)))
        (if (< 2 fd)
            (let ((w (syscall-direct 4 fd 34359738688 4 0 0 0)))
              (let ((mapped (syscall-direct 197 34361835520 4096 1 18 fd 0)))
                (if (= mapped 34361835520)
                    (let ((value (ptr-read-u32 34361835520 0)))
                      (let ((mu (syscall-direct 73 34361835520 4096 0 0 0 0)))
                        (seq
                          (syscall-direct 6 fd 0 0 0 0 0)
                          (syscall-direct 10 34359738624 0 0 0 0 0)
                          (if (= (good w value mu) 1) (ok) (fail)))))
                  (seq
                    (syscall-direct 6 fd 0 0 0 0 0)
                    (syscall-direct 10 34359738624 0 0 0 0 0)
                    (fail)))))
          (fail)))))
  (exit (run)))' 42

# raw Darwin socket(2): create an AF_INET/SOCK_STREAM fd and close it.
# Mirrors the Windows winsock-socket smoke at the Mach-O syscall level.
build_run socket-close '(seq
  (defun run ()
    (let ((fd (syscall-direct 97 2 1 0 0 0 0)))
      (if (< 2 fd)
          (let ((rc (syscall-direct 6 fd 0 0 0 0 0)))
            (if (= rc 0) 42 13))
        13)))
  (exit (run)))' 42

# raw Darwin dup(2)+fcntl(2): create a pipe from x0/x1, duplicate the write
# fd, set FD_CLOEXEC on the duplicate, verify it with F_GETFD, then write
# through the duplicate.
build_run dup-fcntl '(seq
  (defun fail () (syscall-direct 1 13 0 0 0 0 0))
  (defun ok () (syscall-direct 1 42 0 0 0 0 0))
  (defun good-fds (rfd wfd)
    (if (< 2 rfd)
        (if (< 2 wfd) 1 0)
      0))
  (defun good (setfd getfd w n)
    (if (= setfd 0)
        (if (= getfd 1)
            (if (= w 4)
                (if (= n 4)
                    (if (= (ptr-read-u32 34359738368 384) 1718646116) 1 0)
                  0)
              0)
          0)
      0))
  (defun run ()
    (seq
      (syscall-direct 197 34359738368 1048576 3 4114 -1 0)
      (let ((rfd (syscall-direct-store-x1 42 0 0 0 0 0 0 34359738368 256)))
        (let ((wfd (ptr-read-u64 34359738368 256)))
          (if (= (good-fds rfd wfd) 1)
              (let ((dupfd (syscall-direct 41 wfd 0 0 0 0 0)))
                (if (< 2 dupfd)
                    (let ((setfd (syscall-direct 92 dupfd 2 1 0 0 0)))
                      (let ((getfd (syscall-direct 92 dupfd 1 0 0 0 0)))
                        (seq
                          (ptr-write-u32 34359738368 320 1718646116)
                          (let ((w (syscall-direct 4 dupfd 34359738688 4 0 0 0)))
                            (let ((n (syscall-direct 3 rfd 34359738752 4 0 0 0)))
                              (seq
                                (syscall-direct 6 rfd 0 0 0 0 0)
                                (syscall-direct 6 wfd 0 0 0 0 0)
                                (syscall-direct 6 dupfd 0 0 0 0 0)
                                (if (= (good setfd getfd w n) 1)
                                    (ok)
                                  (fail))))))))
                  (fail)))
            (fail))))))
  (exit (run)))' 42

# cons-make-with-clone: fused (alloc box + deep-clone car/cdr).  Clone
# Int(20) into car and Int(3) into cdr, read both back -> 20 + 3 = 23.
# Doc 147 Phase 3: NlConsBox is 24B; car WORD @0, cdr WORD @8.  cons-make-
# with-clone stores each via nl_val_clone_into; cons-car/cdr materialise a
# 32B view via nl_sexp_clone_into.  All stubbed here (compile+run smoke).
build_run cons-clone '(seq
  (defun nl_alloc_bytes (size align) (atomic-fetch-add 34359738368 size))
  (defun nl_alloc_consbox () (nl_alloc_bytes 24 8))
  (defun nl_sexp_clone_into (src dst)
    (seq
      (ptr-write-u64 dst 0 (ptr-read-u64 src 0))
      (ptr-write-u64 dst 8 (ptr-read-u64 src 8))
      (ptr-write-u64 dst 16 (ptr-read-u64 src 16))
      (ptr-write-u64 dst 24 (ptr-read-u64 src 24))))
  (defun nl_val_clone_into (src dst)
    (if (= (logand src 1) 1)
        (ptr-write-u64 dst 0 src)
      (let ((box (nl_alloc_bytes 32 8)))
        (seq (nl_sexp_clone_into src box)
             (ptr-write-u64 dst 0 box)))))
  (defun nl_val_load (word scratch)
    (if (= (logand word 1) 0) word
      (seq (ptr-write-u64 scratch 0 word) scratch)))
  (defun run ()
    (seq
      (syscall-direct 197 34359738368 1048576 3 4114 -1 0)
      (ptr-write-u64 34359738368 0 34359738880)
      (sexp-int-make 34359738432 20)
      (sexp-int-make 34359738496 3)
      (cons-make-with-clone 34359738432 34359738496 34359738560)
      (cons-car 34359738560 34359738752)
      (cons-cdr 34359738560 34359738816)
      (+ (ptr-read-u64 34359738752 8) (ptr-read-u64 34359738816 8))))
  (exit (run)))' 23

# boxed data ops: cell-make/value/set/null, vector make/ref/ref-ptr/set,
# record make/type-tag/slot-ref/slot-ref-ptr/set/count, and cons-cdr-raw.
# The local helpers implement the minimal Vec/NlCell/NlRecord layouts the
# aarch64 emitters read: NlVector data@+8 len@+16; NlRecord data@+40 len@+48.
build_run boxed '(seq
  (defun nl_alloc_bytes (size align) (atomic-fetch-add 34359738368 size))
  (defun nl_sexp_clone_into (src dst)
    (seq
      (ptr-write-u64 dst 0 (ptr-read-u64 src 0))
      (ptr-write-u64 dst 8 (ptr-read-u64 src 8))
      (ptr-write-u64 dst 16 (ptr-read-u64 src 16))
      (ptr-write-u64 dst 24 (ptr-read-u64 src 24))))
  (defun nl_alloc_consbox () (nl_alloc_bytes 24 8))
  (defun nl_alloc_cell (valptr)
    (let ((box (nl_alloc_bytes 16 8)))
      (seq (nl_sexp_clone_into valptr box) box)))
  (defun nl_cell_set_value (box valptr)
    (nl_sexp_clone_into valptr box))
  (defun nl_cell_get_value (cellptr out)
    (nl_sexp_clone_into (ptr-read-u64 (ptr-read-u64 cellptr 8) 0) out))
  ;; Doc 147 Phase 0/2 keystone + slot-ptr stubs (compile-only): store a
  ;; 32B-slot SRC as an 8B WORD / load a WORD back into a 32B view, and
  ;; the vector/record slot-ptr materialisers, so the emitted bl targets
  ;; resolve.  Container data buffers are now 8B-per-slot WORDS.
  (defun nl_val_clone_into (src dst)
    (if (= (logand src 1) 1)
        (ptr-write-u64 dst 0 src)
      (let ((box (nl_alloc_bytes 32 8)))
        (seq (nl_sexp_clone_into src box)
             (ptr-write-u64 dst 0 box)))))
  (defun nl_val_load (word scratch)
    (if (= (logand word 1) 0) word
      (seq (ptr-write-u64 scratch 0 word) scratch)))
  (defun nl_alloc_vector (cap)
    (let ((box (nl_alloc_bytes 32 8)))
      (seq
        (ptr-write-u64 box 8 (nl_alloc_bytes (* cap 8) 8))
        (ptr-write-u64 box 16 cap)
        box)))
  (defun nl_vector_set_slot (vec idx valptr)
    (nl_val_clone_into valptr (+ (ptr-read-u64 vec 8) (* idx 8))))
  (defun nl_vector_slot_ptr (sexpptr idx)
    (nl_val_load
     (ptr-read-u64 (+ (ptr-read-u64 (ptr-read-u64 sexpptr 8) 8) (* idx 8)) 0)
     (nl_alloc_bytes 32 8)))
  (defun nl_alloc_record (tagptr count)
    (let ((box (nl_alloc_bytes 64 8)))
      (seq
        (nl_sexp_clone_into tagptr box)
        (ptr-write-u64 box 40 (nl_alloc_bytes (* count 8) 8))
        (ptr-write-u64 box 48 count)
        box)))
  (defun nl_record_set_slot (rec idx valptr)
    (nl_val_clone_into valptr (+ (ptr-read-u64 rec 40) (* idx 8))))
  (defun nl_record_slot_ptr (sexpptr idx)
    (nl_val_load
     (ptr-read-u64 (+ (ptr-read-u64 (ptr-read-u64 sexpptr 8) 40) (* idx 8)) 0)
     (nl_alloc_bytes 32 8)))
  (defun run ()
    (seq
      (syscall-direct 197 34359738368 1048576 3 4114 -1 0)
      (ptr-write-u64 34359738368 0 34359742464)
      (sexp-int-make 34359738432 5)
      (sexp-int-make 34359738496 17)
      (sexp-int-make 34359738560 19)
      (cell-make 34359738496 34359738624)
      (cell-value 34359738624 34359738688)
      (cell-set-value 34359738624 34359738560)
      (cell-value 34359738624 34359738752)
      (vector-make 2 34359738816)
      (vector-slot-set 34359738816 0 34359738496)
      (vector-slot-set 34359738816 1 34359738560)
      (vector-ref 34359738816 1 34359738880)
      (record-make 34359738432 2 34359738944)
      (record-slot-set 34359738944 0 34359738496)
      (record-slot-set 34359738944 1 34359738560)
      (record-slot-ref 34359738944 0 34359739008)
      (record-type-tag 34359738944 34359739072)
      (cons-make 34359738496 34359738560 34359739136)
      (+ (+ (ptr-read-u64 34359738688 8)
            (ptr-read-u64 34359738752 8))
         (+ (vector-len 34359738816)
            (+ (sexp-int-unwrap (vector-ref-ptr 34359738816 0))
               (+ (ptr-read-u64 34359738880 8)
                  (+ (record-slot-count 34359738944)
                     (+ (sexp-int-unwrap (record-slot-ref-ptr 34359738944 1))
                        (+ (ptr-read-u64 34359739008 8)
                           (+ (ptr-read-u64 34359739072 8)
                              (+ (if (< 0 (sexp-payload-ptr-record 34359738944)) 1 0)
                                 (if (= (cons-cdr-raw 34359739136) 0) 3 0))))))))))))
      (exit (run)))' 121

# string/symbol name ops: hand-build Symbol("ab") and Str("ab"), check
# str-bytes, str-bytes-ptr, str-eq, symbol-eq, symbol-name-eq,
# sexp-name-eq, and sexp-write-nil/t.  Expected:
# 97 + 3 + 5 + 7 + 11 + 0 + 17 + 19 = 159.
build_run names '(seq
  (defun nl_str_bytes_ptr (ptr) (str-bytes ptr))
  (defun run ()
    (seq
      (syscall-direct 197 34359738368 1048576 3 4114 -1 0)
      (ptr-write-u64 34359738624 0 25185)
      (ptr-write-u64 34359738432 0 4)
      (ptr-write-u64 34359738432 16 34359738624)
      (ptr-write-u64 34359738432 24 2)
      (ptr-write-u64 34359738496 0 4)
      (ptr-write-u64 34359738496 16 34359738624)
      (ptr-write-u64 34359738496 24 2)
      (ptr-write-u64 34359738560 0 5)
      (ptr-write-u64 34359738560 16 34359738624)
      (ptr-write-u64 34359738560 24 2)
      (sexp-write-nil 34359738688)
      (sexp-write-t 34359738752)
      (+ (ptr-read-u8 (str-bytes 34359738560) 0)
         (+ (* 3 (str-eq 34359738560 34359738560))
            (+ (* 5 (symbol-eq 34359738432 34359738496))
               (+ (* 7 (symbol-name-eq 34359738432 "ab"))
                  (+ (* 11 (sexp-name-eq 34359738560 "ab"))
                     (+ (* 13 (sexp-tag 34359738688))
                        (+ (* 17 (sexp-tag 34359738752))
                           (* 19 (if (= (str-bytes-ptr 34359738560)
                                        (str-bytes 34359738560)) 1 0)))))))))))
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
      (syscall-direct 197 34359738368 1048576 3 4114 -1 0)
      (write_outs 11 22 34359738752 34359738816)
      (+ 1
         (+ (ptr-read-u64 34359738752 0)
            (ptr-read-u64 34359738816 0)))))
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
      (syscall-direct 197 34359738368 1048576 3 4114 -1 0)
      (ptr-write-u64 34359738624 0 25185)
      (sexp-write-str 34359738432 34359738624 2)
      (sexp-write-symbol 34359738496 34359738624 2)
      (mut-str-make-empty 34359738560 3)
      (str-codepoint-at 34359738432 0 34359738752 34359738816)
      (+ (sexp-tag 34359738432)
         (+ (sexp-tag 34359738496)
            (+ (sexp-tag 34359738560)
               (+ (mut-str-push-byte 34359738560 97)
                  (+ (mut-str-push-codepoint 34359738560 98)
                     (+ (mut-str-len 34359738560)
                        (+ (sexp-tag (mut-str-finalize 34359738560 34359738688))
                           (+ (str-char-count 34359738432)
                              (+ 1
                                 (+ (ptr-read-u64 34359738752 0)
                                    (+ (ptr-read-u64 34359738816 0)
                                       (str-is-alphanumeric-at 34359738432 0))))))))))))))
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
      (syscall-direct 197 34359738368 1048576 3 4114 -1 0)
      (sexp-write-symbol-lit 34359738432 "ab")
      (sexp-write-str-lit 34359738496 "ab")
      (+ (sexp-tag 34359738432)
         (+ (ptr-read-u64 34359738432 8)
            (+ (sexp-tag 34359738496)
               (ptr-read-u64 34359738496 8))))))
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
      (syscall-direct 197 34359738368 1048576 3 4114 -1 0)
      (make-str 34359738432 1 2 3 4 34359738624)
      (sexp-tag 34359738432)))
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
      (syscall-direct 197 34359738368 1048576 3 4114 -1 0)
      (sexp-write-float 34359738432 (i64-to-f64 42))
      (+ (sexp-tag 34359738432)
         (f64-to-i64-trunc
           (bits-to-f64
             (sexp-float-unwrap 34359738432))))))
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
