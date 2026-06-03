#!/usr/bin/env bash
# Stage 3 self-host test: the standalone NeLisp interpreter (target/nelisp,
# a pure-elisp Phase-47-AOT binary, ZERO Rust)
# loads its OWN compiler toolchain as source and compiles a program
# (incl. a recursive `fact') to a native x86_64 ELF — with NO emacs in the
# loop — then we exec that ELF and assert its exit code.
#
# Pipeline proven here:
#   source sexp --(standalone interpreter running nelisp-phase47-compiler)-->
#   native ELF --(kernel exec)--> exit code
#
# Depends on:
#   * scripts/nelisp-stdlib-prelude.el   (cl-some etc.)
#   * lisp/nelisp-phase47-compiler.el    (--native-emit-enabled gate = nil)
#   * lisp/nelisp-asm-x86_64.el          (variadic logior/logand/logxor in the
#                                         standalone interpreter — see
#                                         scripts/nelisp-standalone-build.el)
#   * lisp/nelisp-elf-write.el, lisp/nelisp-static-linker.el
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$here"

RB="target/nelisp"
if [ ! -x "$RB" ]; then
  echo "[selfhost] building reader binary..."
  emacs --batch -Q -L lisp -L src -L scripts \
        --eval '(setq load-prefer-newer t)' \
        -l nelisp-standalone-build -f nelisp-standalone-build-reader >/dev/null 2>&1
fi

driver="$(mktemp /tmp/nelisp-selfhost-XXXXXX.el)"
outbin="$(mktemp /tmp/nelisp-selfhost-bin-XXXXXX)"
trap 'rm -f "$driver" "$outbin"' EXIT

# prelude + toolchain (dependency order) + a POSITIONAL compile entry.
# The positional wrapper exists because `nelisp-phase47-compile-sexp' is a
# `cl-defun' with `&key' defaults, and standalone NeLisp does not yet bind
# &key defaults on a positional call (arch would be nil -> guard signals).
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
WRAP

# compile the recursive `fact' demo, returning sentinel 88 from the driver
printf "(progn (nelisp-selfhost-compile '(seq (defun fact (n) (if (< n 1) 1 (* n (fact (- n 1))))) (exit (fact 5))) \"%s\") 88)\n" "$outbin" >> "$driver"

echo "[selfhost] compiling (fact 5) via standalone interpreter (zero emacs)..."
set +e
"$RB" "$driver"; compile_rc=$?
set -e
if [ "$compile_rc" != "88" ]; then
  echo "[selfhost] FAIL: driver returned $compile_rc (expected 88 = compile completed)"; exit 1
fi
if [ ! -s "$outbin" ]; then
  echo "[selfhost] FAIL: no ELF produced"; exit 1
fi
chmod +x "$outbin"
set +e
"$outbin"; run_rc=$?
set -e
if [ "$run_rc" = "120" ]; then
  echo "[selfhost] PASS: standalone-compiled (fact 5) -> exit 120 (5! = 120)"
  exit 0
else
  echo "[selfhost] FAIL: produced binary exit $run_rc (expected 120)"; exit 1
fi
