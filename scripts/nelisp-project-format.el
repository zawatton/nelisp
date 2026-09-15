;;; nelisp-project-format.el --- Project indentation planner -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Returns a plan only. Does not visit/evaluate project files or local variables.
(require 'json)
(declare-function nelisp-dev-source-symbols "nelisp-dev-source" (text))
(declare-function nelisp-dev-source-completion-symbols "nelisp-dev-source" (text))
(declare-function nelisp-dev-source-reader-builtins "nelisp-dev-source" (text))
(declare-function nelisp-dev-source-lookup "nelisp-dev-source" (text offset &optional parsed unresolved external))
(declare-function nelisp-dev-source-references "nelisp-dev-source" (text offset &optional parsed external))
(declare-function nelisp-dev-source-rename "nelisp-dev-source" (text offset &optional new-name))
(declare-function nelisp-dev-source-signature "nelisp-dev-source" (text offset &optional external))
(declare-function nelisp-dev-source-local-completions "nelisp-dev-source" (text offset &optional external))
(declare-function nelisp-dev-source-occurrences "nelisp-dev-source" (text name namespace &optional external))
(declare-function nelisp-dev-source-test-symbols "nelisp-dev-source" (text))

(defun nelisp-project-format--json-keys (value)
  "Convert protocol string-key alists to keys accepted by `json-serialize'."
  (cond
   ((vectorp value) (vconcat (mapcar #'nelisp-project-format--json-keys value)))
   ((and (consp value) (consp (car value)) (stringp (caar value)))
    (mapcar (lambda (pair)
              (cons (intern (car pair)) (nelisp-project-format--json-keys (cdr pair))))
            value))
   (t value)))

(defun nelisp-project-format--forms ()
  "Read the current buffer as data; never evaluate its forms."
  (goto-char (point-min))
  (let (forms)
    (while (progn
             ;; The Lisp reader accepts CR as whitespace, while this mode's
             ;; syntax table does not. Consume reader whitespace explicitly
             ;; between forms/comments, without rewriting string contents.
             (while (let ((before (point)))
                      (skip-chars-forward " \t\r\n")
                      (forward-comment (point-max))
                      (/= before (point))))
             (not (eobp)))
      (push (read (current-buffer)) forms))
    (nreverse forms)))

(defun nelisp-project-format--external (item &optional shared)
  "Normalize ITEM's callable keys, or select providers from SHARED context."
  (if (eq (alist-get 'workspace_context item) t)
      (progn
        (unless (and shared (not (assq 'external item)))
          (error "Shared workspace context requires a table and no inline declarations"))
        (apply #'append
               (mapcar (lambda (group)
                         (unless (equal (car group) (alist-get 'path item)) (cdr group)))
                       (cdr shared))))
    (mapcar (lambda (definition)
              (mapcar (lambda (pair) (cons (symbol-name (car pair)) (cdr pair))) definition))
            (alist-get 'external item))))

(defun nelisp-project-format--diagnostic (item)
  "Return the first syntax diagnostic for ITEM, or nil.
Line and column are one-based Unicode character positions, not display cells."
  (with-temp-buffer
    (insert (alist-get 'text item))
    (emacs-lisp-mode)
    (condition-case err
        (progn
          (let ((inhibit-message t)) (check-parens))
          (nelisp-project-format--forms)
          nil)
      (error
       `((path . ,(alist-get 'path item))
         (severity . "error") (code . "NELISP-SYNTAX")
         (condition . ,(symbol-name (car err)))
         (message . ,(error-message-string err))
         (line . ,(line-number-at-pos))
         (column . ,(1+ (- (point) (line-beginning-position)))))))))

(defun nelisp-project-format-main ()
  "Read JSON snapshots, validate all, and return an indentation plan."
  (condition-case err
      (let* ((input (with-temp-buffer
                      (insert-file-contents (getenv "NELISP_FORMAT_INPUT"))
                      (json-parse-buffer :object-type 'alist :array-type 'list)))
             (shared-callables
              (when (assq 'workspace_callables (car input))
                (cons :workspace
                      (cons (cons :builtins
                                  (nelisp-project-format--external
                                   (list (cons 'external (alist-get 'workspace_builtins (car input))))))
                            (mapcar (lambda (group)
                                      (cons (alist-get 'path group) (nelisp-project-format--external group)))
                                    (alist-get 'workspace_callables (car input)))))))
             (result
              (if (getenv "NELISP_FORMAT_LOOKUP")
                  (let ((occurrence-count 0))
                    (require 'nelisp-dev-source)
                    (mapcar (lambda (item)
                              `((path . ,(alist-get 'path item))
                                (lookup . ,(or (nelisp-project-format--json-keys
                                                (cond ((alist-get 'builtin_catalog item)
                                                       (nelisp-dev-source-reader-builtins (alist-get 'text item)))
                                                      ((alist-get 'local_scope item)
                                                       (nelisp-dev-source-local-completions
                                                        (alist-get 'text item) (alist-get 'offset item)
                                                        (nelisp-project-format--external item shared-callables)))
                                                      ((getenv "NELISP_FORMAT_OCCURRENCES")
                                                       (let ((found (nelisp-dev-source-occurrences (alist-get 'text item)
                                                                                                  (alist-get 'symbol item)
                                                                                                  (alist-get 'namespace item)
                                                                                                  (nelisp-project-format--external item shared-callables))))
                                                         (setq occurrence-count (+ occurrence-count (length found)))
                                                         (when (> occurrence-count 20000) (error "Workspace references exceed 20000 locations"))
                                                         found))
                                                      ((getenv "NELISP_FORMAT_SIGNATURE")
                                                       (nelisp-dev-source-signature
                                                        (alist-get 'text item) (alist-get 'offset item)
                                                        (nelisp-project-format--external item shared-callables)))
                                                      ((getenv "NELISP_FORMAT_RENAME")
                                                    (nelisp-dev-source-rename (alist-get 'text item) (alist-get 'offset item)
                                                                              (alist-get 'new_name item)))
                                                      ((getenv "NELISP_FORMAT_REFERENCES")
                                                       (nelisp-dev-source-references (alist-get 'text item) (alist-get 'offset item)
                                                                                     nil (nelisp-project-format--external item shared-callables)))
                                                      (t
                                                       (nelisp-dev-source-lookup (alist-get 'text item) (alist-get 'offset item)
                                                                                 nil (alist-get 'unresolved item)
                                                                                 (nelisp-project-format--external item shared-callables))))) :null))))
                            input))
              (if (or (getenv "NELISP_FORMAT_SYMBOLS") (getenv "NELISP_FORMAT_COMPLETIONS") (getenv "NELISP_FORMAT_TESTS"))
                  (progn
                    (require 'nelisp-dev-source)
                    (mapcar (lambda (item)
                              `((path . ,(alist-get 'path item))
                                (symbols . ,(nelisp-project-format--json-keys
                                             (cond ((getenv "NELISP_FORMAT_TESTS")
                                                    (nelisp-dev-source-test-symbols (alist-get 'text item)))
                                                   ((getenv "NELISP_FORMAT_COMPLETIONS")
                                                    (nelisp-dev-source-completion-symbols (alist-get 'text item)))
                                                   (t (nelisp-dev-source-symbols (alist-get 'text item))))))))
                            input))
              (if (getenv "NELISP_FORMAT_DIAGNOSTICS")
                  `((checked_files . ,(length input))
                    (diagnostics . ,(vconcat
                                     (delq nil (mapcar #'nelisp-project-format--diagnostic input)))))
              (mapcar
               (lambda (item)
                 (with-temp-buffer
                   (insert (alist-get 'text item))
                   (emacs-lisp-mode)
                   (setq-local indent-tabs-mode nil)
                   (check-parens)
                   (let ((before (nelisp-project-format--forms)))
                     (unless (getenv "NELISP_FORMAT_VALIDATE_ONLY")
                       (let ((inhibit-message t))
                         (indent-region (point-min) (point-max))))
                     (unless (equal before (nelisp-project-format--forms))
                       (error "Indentation would change Lisp data in %s"
                              (alist-get 'path item))))
                   `((path . ,(alist-get 'path item)) (text . ,(buffer-string)))))
               input))))))
        (let ((coding-system-for-write 'utf-8-unix))
          (princ (decode-coding-string
                  (json-serialize (if (getenv "NELISP_FORMAT_DIAGNOSTICS") result (vconcat result)))
                  'utf-8-unix)))
        (kill-emacs 0))
    (error (message "nelisp fmt: %s" (error-message-string err))
           (kill-emacs 1))))

(provide 'nelisp-project-format)
;;; nelisp-project-format.el ends here
