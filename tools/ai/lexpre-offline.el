;;; lexpre-offline.el --- host-side red/green for the lexical prelude selector  -*- lexical-binding: t; -*-
;; usage: emacs --batch -l tools/ai/lexpre-offline.el --eval '(lexpre-offline "SELECTOR.el" "REPO-ROOT")'
;; Prints one line per census name: NAME ARGLIST STATUS OPCODES.
(require 'cl-lib)
(require 'bytecomp)
(defun lexpre-offline--defun-text (root name)
  (with-temp-buffer
    (insert-file-contents (expand-file-name "scripts/nelisp-stdlib-prelude.el" root))
    (goto-char (point-min))
    (when (re-search-forward (format "^ *(defun %s " (regexp-quote (symbol-name name))) nil t)
      (goto-char (match-beginning 0))
      (let ((start (point)))
        (forward-sexp 1)
        (buffer-substring-no-properties start (point))))))
(defun lexpre-offline (selector root)
  (setq nelisp-prelude-bytecode-repo-root root)
  (load (expand-file-name selector root) nil t)
  (let* ((fixtures (nelisp-prelude-bytecode-read-parity-fixtures
                    (expand-file-name "test/nelisp-prelude-bytecode-parity-fixtures.tsv" root)))
         (names '(identity purecopy car-safe cdr-safe markerp obarrayp int-to-string
                  decoded-time-minute decoded-time-second
                  file-attribute-modification-time file-attribute-size
                  nelisp--record-type nelisp--record-ref nelisp--prn-float
                  string-to-vector proper-list-p max-char file-remote-p
                  make-sparse-keymap standard-syntax-table))
         (src (mapconcat (lambda (n) (or (lexpre-offline--defun-text root n) "")) names "\n"))
         (res (nelisp-prelude-bytecode-transform src "scripts/nelisp-stdlib-prelude.el" fixtures)))
    (dolist (row (nth 2 res))
      (when (memq (nth 2 row) names)
        (let* ((text (nth 0 res)))
          (ignore text)
          (princ (format "ROW %s %s %s\n" (nth 2 row) (nth 3 row) (nth 4 row))))))
    ;; arglist actually emitted
    (with-temp-buffer
      (insert (nth 0 res)) (goto-char (point-min))
      (condition-case nil
          (while t
            (let ((f (read (current-buffer))))
              (when (eq (car-safe f) 'prog1)
                (let* ((mk (nth 2 (nth 2 f))))
                  (princ (format "CELL %s arglist=%S\n" (cadr (nth 1 f)) (nth 1 mk)))))))
        (end-of-file nil)))))
