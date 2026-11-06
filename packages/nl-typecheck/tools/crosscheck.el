;;; crosscheck.el --- write target/typecheck/prims-crosscheck.txt -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'nl-typecheck-prims)

(let* ((file (or (getenv "TYPECHECK_CROSSCHECK_OUT")
                 "target/typecheck/prims-crosscheck.txt"))
       (lines (nl-typecheck-prims-crosscheck)))
  (make-directory (file-name-directory (expand-file-name file)) t)
  (with-temp-file file
    (insert (format "# nl-typecheck prims cross-check: %d table entries, %d NeLisp builtins, %d disagreements\n"
                    (length nl-typecheck-prims-table)
                    (length nl-typecheck-prims-nelisp-builtins)
                    (length lines)))
    (dolist (l lines) (insert l "\n")))
  (message "crosscheck: %d disagreements -> %s" (length lines) file))
