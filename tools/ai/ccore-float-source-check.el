;;; ccore-float-source-check.el --- Verify the float DSL source before a build -*- lexical-binding: t; -*-
;; Usage: emacs -Q --batch -l tools/ai/ccore-float-source-check.el
;; Optional override: NELISP_FLOAT_SOURCE=/path/to/source.el. Relative paths
;; are resolved from this script's directory.

(require 'seq)

(defconst ccore-float-source-check--script-dir
  (file-name-directory (or load-file-name buffer-file-name)))
(defconst ccore-float-source-check--source
  (expand-file-name
   (or (getenv "NELISP_FLOAT_SOURCE")
       "../../lisp/nelisp-cc-evalport-str-to-float.el")
   ccore-float-source-check--script-dir))

(defun ccore-float-source-check--read-file (path)
  (with-temp-buffer
    (insert-file-contents path)
    (emacs-lisp-mode)
    (check-parens)
    (goto-char (point-min))
    (condition-case err
        (while t (read (current-buffer)))
      (end-of-file
       (unless (eobp)
         (error "Incomplete source form at character %d" (point))))
      (error (signal (car err) (cdr err))))))

(defvar ccore-float-source-check--definitions nil)

(defun ccore-float-source-check--collect (tree)
  (when (consp tree)
    (when (and (eq (car tree) 'defun) (symbolp (cadr tree)))
      (push tree ccore-float-source-check--definitions))
    (ccore-float-source-check--collect (car tree))
    (ccore-float-source-check--collect (cdr tree))))

(defun ccore-float-source-check--contains (tree symbol)
  (or (eq tree symbol)
      (and (consp tree)
           (or (ccore-float-source-check--contains (car tree) symbol)
               (ccore-float-source-check--contains (cdr tree) symbol)))))

(defun ccore-float-source-check--run ()
  (unless (file-readable-p ccore-float-source-check--source)
    (error "Source file is not readable: %s" ccore-float-source-check--source))
  (ccore-float-source-check--read-file ccore-float-source-check--source)
  (load-file ccore-float-source-check--source)
  (setq ccore-float-source-check--definitions nil)
  (ccore-float-source-check--collect
   nelisp-cc-evalport-str-to-float--source)
  (let* ((finish-functions
          (seq-filter (lambda (form) (eq (cadr form) 'nl_stf_finish))
                      ccore-float-source-check--definitions))
         (finish (car finish-functions))
         (short-loop
          (seq-find (lambda (form) (eq (cadr form) 'nl_short_loop))
                    ccore-float-source-check--definitions))
         (result-builder
          (seq-find (lambda (form) (eq (cadr form) 'nl_stf_build_result))
                    ccore-float-source-check--definitions)))
    (unless (= (length finish-functions) 1)
      (error "Expected one active nl_stf_finish definition; found %d"
             (length finish-functions)))
    (unless (and short-loop
                 (ccore-float-source-check--contains short-loop 'nl_stf_finish)
                 result-builder
                 (ccore-float-source-check--contains
                  result-builder 'nl_stf_finish))
      (error "Known callers do not resolve to nl_stf_finish"))
    (unless (and (ccore-float-source-check--contains
                  finish 'nlf_exact_fallback)
                 (ccore-float-source-check--contains finish '>=)
                 (ccore-float-source-check--contains finish '<=)
                 (ccore-float-source-check--contains finish -27)
                 (ccore-float-source-check--contains finish 55))
      (error "Active nl_stf_finish lacks the bounded exact fallback"))
    (princ (format "ccore-float-source-check: PASS (%s)\n"
                   ccore-float-source-check--source))))

(ccore-float-source-check--run)

;;; ccore-float-source-check.el ends here
