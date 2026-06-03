#!/usr/bin/env bash
# Stage 4 self-host MULTI-THREADED test.  The standalone NeLisp interpreter
# (pure-elisp Phase-47-AOT binary, ZERO Rust, ZERO emacs) loads its own
# compiler toolchain as source and compiles a MULTI-THREADED program to a
# native x86_64 ELF.  The compiled program spawns N real OS threads via the
# raw clone(2) syscall (CLONE_VM|CLONE_FS|CLONE_FILES), each thread runs a
# `worker' defun (its own frame on its own clone stack) that writes a partial
# result into a shared mmap region and bumps a shared atomic done-counter
# (SeqCst atomic-fetch-add); the parent joins by spin-waiting on that counter
# and reduces the partials.  This proves NeLisp's multi-threaded parallel
# build capability end to end: self-host compile -> parallel native execution.
#
#   3 workers write 14 each into distinct slots -> parent sum = 42.
#   A wrong result (one thread not run / a torn write / a lost atomic) would
#   not be 42, so the assertion is meaningful.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$here"

RB="target/nelisp"
if [ ! -x "$RB" ]; then
  echo "[selfhost-mt] building reader binary..."
  emacs --batch -Q -L lisp -L src -L scripts \
        --eval '(setq load-prefer-newer t)' \
        -l nelisp-standalone-build -f nelisp-standalone-build-reader >/dev/null 2>&1
fi

driver="$(mktemp /tmp/nelisp-selfhost-mt-XXXXXX.el)"
outbin="$(mktemp /tmp/nelisp-selfhost-mt-bin-XXXXXX)"
trap 'rm -f "$driver" "$outbin"' EXIT

cat scripts/nelisp-stdlib-prelude.el \
    lisp/nelisp-phase47-compiler.el \
    lisp/nelisp-asm-x86_64.el \
    lisp/nelisp-elf-write.el \
    lisp/nelisp-static-linker.el > "$driver"

cat >> "$driver" <<'WRAP'
(defun nelisp-selfhost-compile (sexp file-path)
  "Positional x86_64 / _start self-host entry (mirrors compile-sexp body)."
  (let* ((nelisp-phase47-compiler--label-counter 0)
         (nelisp-phase47-compiler--arch 'x86_64)
         (ir (nelisp-phase47-compiler--parse sexp nil))
         (collected (nelisp-phase47-compiler--collect-strings ir))
         (str-offsets (car collected))
         (str-rodata-bytes (cdr collected))
         (table-collected (nelisp-phase47-compiler--collect-tables ir))
         (table-offsets (car table-collected))
         (table-bytes (cdr table-collected))
         (str-rodata-len (length str-rodata-bytes))
         (rodata-bytes (concat str-rodata-bytes table-bytes))
         (defuns (nelisp-phase47-compiler--collect-defuns ir))
         (pass1-table-vaddrs (mapcar (lambda (e) (cons (car e) 0)) table-offsets))
         (pass1 (nelisp-phase47-compiler--pass ir defuns str-offsets 0 pass1-table-vaddrs))
         (text-size (nelisp-asm-x86_64-buffer-pos pass1))
         (rodata-vaddr (+ nelisp-phase47-compiler--text-vaddr text-size))
         (table-vaddrs (mapcar (lambda (e)
                          (let* ((nm (car e)) (info (cdr e)) (off (plist-get info :offset)))
                            (cons nm (+ rodata-vaddr str-rodata-len off)))) table-offsets))
         (pass2 (nelisp-phase47-compiler--pass ir defuns str-offsets rodata-vaddr table-vaddrs))
         (text-bytes (nelisp-asm-x86_64-resolve-fixups pass2))
         (have-rodata (> (length rodata-bytes) 0))
         (symbols (cons (list :name "_start" :value 0 :size (length text-bytes)
                              :section 'text :bind 'global :type 'func)
                        (if have-rodata
                            (list (list :name "rodata_blob" :value 0 :size (length rodata-bytes)
                                        :section 'rodata :bind 'local :type 'object))
                          nil)))
         (sections (list :text text-bytes
                         :rodata (if have-rodata rodata-bytes (unibyte-string))
                         :symbols symbols :entry-sym "_start" :machine 'x86_64)))
    (nelisp-elf-write-binary file-path sections)
    file-path))

(progn
  (nelisp-selfhost-compile
   '(seq
     ;; worker: own frame on its own clone stack; write partial -> shared slot,
     ;; bump the shared atomic done-counter (offset 64), exit the task.
     (defun worker (sh slot val)
       (seq (ptr-write-u64 sh slot val)
            (atomic-fetch-add (+ sh 64) 1)
            (syscall-direct 60 0 0 0 0 0 0)))
     (defun run ()
       (let ((shared (syscall-direct 9 0 65536 3 34 -1 0)))
         (let ((a (syscall-direct 9 0 65536 3 34 -1 0)))
           (let ((b (syscall-direct 9 0 65536 3 34 -1 0)))
             (let ((c (syscall-direct 9 0 65536 3 34 -1 0)))
               (seq
                ;; clone(2): CLONE_VM|CLONE_FS|CLONE_FILES=1792, child stack = base+64K
                (if (= (syscall-direct 56 1792 (+ a 65536) 0 0 0 0) 0) (worker shared 0 14) 0)
                (if (= (syscall-direct 56 1792 (+ b 65536) 0 0 0 0) 0) (worker shared 8 14) 0)
                (if (= (syscall-direct 56 1792 (+ c 65536) 0 0 0 0) 0) (worker shared 16 14) 0)
                ;; join: spin-wait until all 3 workers bumped the counter
                (while (< (ptr-read-u64 (+ shared 64) 0) 3) 0)
                ;; reduce the 3 partials
                (+ (ptr-read-u64 shared 0) (+ (ptr-read-u64 shared 8) (ptr-read-u64 shared 16)))))))))
     (exit (run)))
   "OUTBIN_PLACEHOLDER")
  88)
WRAP

sed -i "s#OUTBIN_PLACEHOLDER#$outbin#" "$driver"

echo "[selfhost-mt] compiling a 3-thread clone(2)+atomics program via standalone interpreter (zero emacs)..."
set +e
"$RB" "$driver"; compile_rc=$?
set -e
if [ "$compile_rc" != "88" ]; then
  echo "[selfhost-mt] FAIL: driver returned $compile_rc (expected 88)"; exit 1
fi
if [ ! -s "$outbin" ]; then
  echo "[selfhost-mt] FAIL: no ELF produced"; exit 1
fi
chmod +x "$outbin"
# run a few times: real threads -> guard against flakiness, demand 42 every time
for i in 1 2 3 4 5; do
  set +e; "$outbin"; rc=$?; set -e
  if [ "$rc" != "42" ]; then
    echo "[selfhost-mt] FAIL: run $i -> exit $rc (expected 42 = 3 parallel workers x 14)"; exit 1
  fi
done
echo "[selfhost-mt] PASS: standalone-compiled 3-thread clone(2)+atomics program -> exit 42 x5 (parallel)"
exit 0
