;;; boot-load-apportion.el --- Opt-in boot load cost instrumentation -*- lexical-binding: t; -*-
;; Build with a separate NELISP_STANDALONE_READER_OUTPUT, COLD_IMAGE=0,
;; -L lisp -L src -L scripts -l tools/ai/boot-load-apportion.el.
;; NELISP_BOOT_SKIP omits one provider; NELISP_BOOT_STOP truncates both
;; sides immediately after the selected load. This isolates required
;; providers without evaluating a dependent form after their omission.
;; Neither variable is recognized by ordinary production binaries.
(require 'nelisp-standalone-build)

(defun boot-apportion-walk (form)
  (let* ((feature (cond ((equal form '(load nelisp-standalone--backquote-file)) "backquote")
                        ((equal form '(load 'byte-run)) "byte-run")
                        ((and (eq (car-safe form) 'require)
                              (eq (car-safe (cadr form)) 'quote)
                              (memq (cadr (cadr form)) '(inline json easy-mmode rx cl-seq nelisp-process-buffer)))
                         (symbol-name (cadr (cadr form)))))))
    (cond
     (feature
      `(progn
         ,@(when (equal feature "inline")
             '((when (equal (cdr (assoc "NELISP_BOOT_SKIP" nelisp--environment)) "macroexp")
                 (provide 'macroexp))))
         (if (equal (cdr (assoc "NELISP_BOOT_SKIP" nelisp--environment)) ,feature)
             ,(if (equal feature "inline") '(progn (require 'macroexp) (provide 'inline))
                `(provide ',(intern feature)))
           ,form)
         (when (equal (cdr (assoc "NELISP_BOOT_STOP" nelisp--environment)) ,feature)
           (exit 0))))
     ((or (not (consp form)) (eq (car form) 'quote)) form)
     (t (let (items)
          (while (consp form)
            (push (boot-apportion-walk (car form)) items)
            (setq form (cdr form)))
          (nconc (nreverse items) form))))))

(defun boot-apportion-source (source)
  (with-temp-buffer
    (insert source)
    (goto-char (point-min))
    (condition-case nil
        (while t
          (forward-comment (buffer-size))
          (let* ((start (point)) (form (read (current-buffer))) (end (point))
                 (replacement (boot-apportion-walk form)))
            (unless (equal replacement form)
              (delete-region start end)
              (goto-char start)
              (insert (prin1-to-string replacement) "\n"))))
      (end-of-file nil))
    (goto-char (point-max))
    (insert "\n(princ \"BOOT-LOAD-HISTORY:\") (prin1 load-history) (princ \"\\n\")\n")
    (buffer-string)))

(advice-add 'nelisp-standalone--reader-repl-prelude-source :filter-return #'boot-apportion-source)
(nelisp-standalone-build-reader)
