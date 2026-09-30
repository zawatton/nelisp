#!/usr/bin/env bash
set -euo pipefail
root=$(cd "$(dirname "$0")/.." && pwd -P)
bin=${NELISP_BIN:-${ELN_PROGRESS_BIN:-$root/target/nelisp}}
mode=${1:?usage: nelisp-bytecomp-s3-check.sh core|deletion}
[[ -x $bin ]] || { echo "NELISP_BIN is not executable: $bin" >&2; exit 2; }
tmp=$(mktemp -d "${TMPDIR:-/tmp}/nelisp-s3.XXXXXX")
trap 'rmdir "$tmp" 2>/dev/null || true' EXIT
cat >"$tmp/run.el" <<'EL'
;;; -*- lexical-binding: t; -*-
(let* ((mode (getenv "NELISP_S3_MODE"))
       (names '(current-buffer point point-min point-max goto-char forward-char insert buffer-substring
                erase-buffer buffer-string write-region get-buffer-create
                set-buffer with-current-buffer))
       (saved (mapcar (lambda (name) (cons name (symbol-function name))) names)))
  (when (member mode '("core" "deletion"))
    (dolist (cell saved) (fmakunbound (car cell)))
    (dolist (pair '((current-buffer . nelisp--scratch-current-buffer)
                    (point . nelisp--scratch-point)
                    (point-min . nelisp--scratch-point-min)
                    (point-max . nelisp--scratch-point-max)
                    (goto-char . nelisp--scratch-goto-char)
                    (forward-char . nelisp--scratch-forward-char)
                    (buffer-substring . nelisp--scratch-buffer-substring)
                    (insert . nelisp--scratch-insert)
                    (erase-buffer . nelisp--scratch-erase-buffer)
                    (buffer-string . nelisp--scratch-buffer-string)
                    (write-region . nelisp--scratch-write-region)
                    (get-buffer-create . nelisp--scratch-buffer)
                    (set-buffer . nelisp--scratch-set-buffer)))
      (fset (car pair) (symbol-function (cdr pair))))
    (eval '(defmacro with-current-buffer (buffer &rest body)
             (declare (indent 1) (debug t))
             `(let ((nelisp-buffer--current ,buffer)) ,@body))))
  (require 'bytecomp)
  (load "test/fixtures/s6-corpus/byte-compile-form.wrapper.el" nil t)
  (let ((forms (with-temp-buffer
                 (insert-file-contents "test/fixtures/s6-corpus/byte-compile-form.el")
                 (read (current-buffer)))))
    (princ "S3_FORMS=")
    (prin1 (let ((byte-compile-warnings nil))
             (mapcar (lambda (args)
                       (s6-corpus--byte-compile-form (car args) (cadr args))) forms)))
    (terpri)))
EL
cd "$root"
if ! NELISP_S3_MODE=$mode "$bin" -L "$root/vendor/emacs-lisp/emacs-lisp" -l "$tmp/run.el" >"$tmp/out" 2>"$tmp/err"; then
  cat "$tmp/out" >&2
  cat "$tmp/err" >&2
  exit 1
fi
if ! grep -q '^S3_FORMS=.(.*' "$tmp/out"; then
  cat "$tmp/out" >&2
  cat "$tmp/err" >&2
  echo 'S3 bytecomp process did not emit a complete corpus result' >&2
  exit 1
fi
if [[ -s $tmp/err ]]; then cat "$tmp/err" >&2; exit 1; fi
grep '^S3_FORMS=' "$tmp/out" >"$tmp/standalone.forms"
if [[ $mode == deletion ]]; then
  # Restoring the image's real function cells models loading the buffer API
  # package over the optional scratch aliases.
  if ! NELISP_S3_MODE=api "$bin" -L "$root/vendor/emacs-lisp/emacs-lisp" -l "$tmp/run.el" >"$tmp/api.out" 2>"$tmp/api.err"; then
    cat "$tmp/api.out" >&2; cat "$tmp/api.err" >&2; exit 1
  fi
  if [[ -s $tmp/api.err ]]; then cat "$tmp/api.err" >&2; exit 1; fi
  grep '^S3_FORMS=' "$tmp/api.out" >"$tmp/api.forms"
  diff -u "$tmp/standalone.forms" "$tmp/api.forms"
  echo 'S3.5 PASS scratch aliases vs restored API function cells'
else
  host="$(NELISP_S3_MODE=api "$EMACS_BIN" --batch -Q -L "$root" -l "$tmp/run.el" 2>"$tmp/host.err")"
  [[ ! -s $tmp/host.err ]] || { cat "$tmp/host.err" >&2; exit 1; }
  printf '%s\n' "$host" | grep '^S3_FORMS=' >"$tmp/host.forms"
  diff -u "$tmp/host.forms" "$tmp/standalone.forms"
  echo 'S3.4 PASS scratch-layer results byte-identical to GNU Emacs'
fi
