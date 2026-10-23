#!/usr/bin/env bash
# symbols-name-word-arm64-defect.sh [REV]: the latent aarch64 defect of eval/core 63ec9640e, measured.
#
# At 63ec9640e only the x86-64 name operations redirected a symbol object WORD to its name cell
# (emit-symbol-object-redirect, D9 stage 3); the aarch64 ones sent a WORD with low bits 110 through
# emit-unless-view-arm64, so symbol-name-eq answered false for every interned symbol.  This compiles one
# probe program -- a symbol object whose name cell holds a legacy string view of "abc", compared with
# "abc" -- for both architectures, once with the emitters of REV (default 63ec9640e) and once with the
# working tree's, and prints the exit status of each run (0: the object compared equal to its own name).
# x86-64 runs natively, aarch64 under qemu-aarch64.  With this branch's emitters the name cell must hold a
# String WORD (design A), so the legacy-view-named object is false on both architectures; the
# String-WORD-named object of test/nelisp-aot-symbol-name-word-test.el is true on both.
cd "$(dirname "$0")/../.." || exit 2
rev=${1:-63ec9640e}
dir=$(mktemp -d "${TMPDIR:-/tmp}/snw-defect-XXXXXX") || exit 2
trap 'rm -rf "$dir"' EXIT
git show "$rev:lisp/nelisp-aot-compiler.el" > "$dir/nelisp-aot-compiler.el" || exit 2
cat > "$dir/probe.el" <<'PROBE'
;;; -*- lexical-binding: t; -*-
(load (expand-file-name "test/nelisp-aot-symbol-name-word-test.el") nil t)
(let* ((str nelisp-snw-test--str) (view nelisp-snw-test--view-addr) (cell nelisp-snw-test--cell-addr)
       (obj nelisp-snw-test--obj) (word nelisp-snw-test--word)
       (cases (list (list "legacy-view-named object" 1
                          (append (nelisp-snw-test--bytes str "abc")
                                  (nelisp-snw-test--view view 5 `(+ ,str 64) 3)
                                  (nelisp-snw-test--cell cell 4 view)
                                  (list `(symbol-name-eq ,obj "abc"))))
                    (list "String-WORD-named object" 1
                          (append (nelisp-snw-test--string str "abc")
                                  (nelisp-snw-test--cell cell 4 word)
                                  (list `(symbol-name-eq ,obj "abc")))))))
  (dolist (c cases)
    (dolist (arch '(x86_64 aarch64))
      (princ (format "%s %-28s %-8s exit status %S\n" (getenv "SNW_LABEL") (car c) arch
                     (nelisp-snw-test--run arch (list c)))))))
PROBE
for which in "$rev:$dir" "working tree:lisp"; do
  label=${which%%:*}; path=${which#*:}
  (ulimit -v 8388608; SNW_LABEL="$label" timeout 250 "${GNU_EMACS:-/usr/local/bin/emacs}" --batch -Q -L "$path" -L lisp -L src -L scripts -L test \
    --eval '(setq load-prefer-newer t)' -l ert -l "$dir/probe.el" 2>&1 | grep -E "exit status")
done
