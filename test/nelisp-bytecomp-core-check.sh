#!/usr/bin/env bash
set -euo pipefail
root=$(cd "$(dirname "$0")/.." && pwd -P)
bin=${NELISP_BIN:-$root/target/nelisp}
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
cat > "$tmp/check.el" <<'EL'
(let ((original (symbol-function 'require)) (compile-required nil))
  (fset 'require
        (lambda (feature &optional file noerror)
          (when (eq feature 'compile) (setq compile-required t))
          (funcall original feature file noerror)))
  (require 'bytecomp)
  (load "test/fixtures/s6-corpus/byte-compile-form.wrapper.el" nil t)
  (with-temp-buffer
    (insert-file-contents "test/fixtures/s6-corpus/byte-compile-form.el")
    (dolist (args (read (current-buffer)))
      (s6-corpus--byte-compile-form (car args) (cadr args))))
  (when (or compile-required (featurep 'compile))
    (error "bytecomp path unexpectedly required compile"))
  (princ "S3.3 PASS compile-required=nil featurep=nil\n"))
EL
(cd "$root" && "$bin" -L "$root/vendor/emacs-lisp/emacs-lisp" --load "$tmp/check.el") > "$tmp/out" 2> "$tmp/err"
grep -q '^S3.3 PASS compile-required=nil featurep=nil' "$tmp/out"
if [[ -s "$tmp/err" ]] && ! grep -q "Warning: reference to free variable.*some-var" "$tmp/err"; then
  cat "$tmp/err" >&2
  exit 1
fi
cat "$tmp/out"
