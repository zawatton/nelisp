;;; nelisp-project-format.el --- Project indentation planner -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Returns a plan only. Does not visit/evaluate project files or local variables.
(require 'json)
(declare-function nelisp-dev-source-symbols "nelisp-dev-source" (text))

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
    (while (progn (forward-comment (point-max)) (not (eobp)))
      (push (read (current-buffer)) forms))
    (nreverse forms)))

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
             (result
              (if (getenv "NELISP_FORMAT_SYMBOLS")
                  (progn
                    (require 'nelisp-dev-source)
                    (mapcar (lambda (item)
                              `((path . ,(alist-get 'path item))
                                (symbols . ,(nelisp-project-format--json-keys
                                             (nelisp-dev-source-symbols (alist-get 'text item))))))
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
               input)))))
        (let ((coding-system-for-write 'utf-8-unix))
          (princ (decode-coding-string
                  (json-serialize (if (getenv "NELISP_FORMAT_DIAGNOSTICS") result (vconcat result)))
                  'utf-8-unix)))
        (kill-emacs 0))
    (error (message "nelisp fmt: %s" (error-message-string err))
           (kill-emacs 1))))

(provide 'nelisp-project-format)
;;; nelisp-project-format.el ends here
