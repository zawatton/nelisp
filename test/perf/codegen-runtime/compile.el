;;; compile.el --- compile pinned runtime fixtures -*- lexical-binding: t; -*-
(require 'json)
(require 'nelisp-standalone-build)
(defconst codegen-runtime--directory
  (file-name-directory (or load-file-name buffer-file-name)))
(defconst codegen-runtime--out
  (or (getenv "CODEGEN_RUNTIME_OUT")
      (expand-file-name "target/codegen/s5/runtime" default-directory)))

(defun codegen-runtime--forms (name)
  (with-temp-buffer
    (insert-file-contents (expand-file-name name codegen-runtime--directory))
    (goto-char (point-min))
    (let (result form)
      (while (setq form (condition-case nil (read (current-buffer))
                          (end-of-file nil)))
        (push form result))
      (nreverse result))))

(defun codegen-runtime-unit ()
  "Compile all pinned functions through the standalone unit pipeline."
  (let* ((nelisp-standalone--target 'linux-x86_64)
         (source (cons 'seq (codegen-runtime--forms "runtime.nl.el")))
         (unit (nelisp-standalone--compile-to-unit "runtime-study" source))
         (text (cdr (assq 'text (plist-get unit :sections))))
         (symbols (sort (cl-remove-if-not
                         (lambda (s) (eq (plist-get s :section) 'text))
                         (copy-sequence (plist-get unit :symbols)))
                        (lambda (a b) (< (plist-get a :value) (plist-get b :value)))))
         rows)
    (make-directory codegen-runtime--out t)
    (with-temp-file (expand-file-name "runtime.nl.text" codegen-runtime--out)
      (set-buffer-multibyte nil) (insert text))
    (while symbols
      (let* ((s (pop symbols)) (lo (plist-get s :value))
             (end (if symbols (plist-get (car symbols) :value) (length text))))
        (push (cons (plist-get s :name)
                    `((offset . ,lo) (size . ,(- end lo)))) rows)))
    (with-temp-file (expand-file-name "runtime.nl.symbols.json" codegen-runtime--out)
      (insert (json-encode rows)))
    (princ (format "Compiled %d pinned functions\n" (length rows)))))

(defun codegen-runtime-executable (name)
  "Compile the selected pinned functions and representative driver."
  (let* ((spec (json-read-file (expand-file-name "fixtures.json" codegen-runtime--directory)))
         (entry (assq (intern name) spec))
         (wanted (append (cdr (assq 'functions (cdr entry))) nil))
         (source (cl-remove-if-not (lambda (f) (memq (cadr f) (mapcar #'intern wanted)))
                                   (codegen-runtime--forms "runtime.nl.el")))
         (extra (codegen-runtime--forms (concat name ".nl.el")))
         (harness (codegen-runtime--forms "harness.nl.el")))
    ;; Identical within-unit small-defun inlining to the runtime build.
    ;; AOT's executable frontend supplies _start and OS entry handling.
    (let ((addresses (cdr (assq 'data_addresses (cdr entry)))))
      (cl-labels ((rewrite (x)
                    (cond ((not (consp x)) x)
                          ((and (eq (car x) 'data-addr) (assq (cadr x) addresses))
                           (cdr (assq (cadr x) addresses)))
                          (t (mapcar #'rewrite x)))))
        (setq source (rewrite (cons 'seq (append source harness extra '((exit (cr_start)))))))))
    (setq source (nelisp-standalone--inline-small-defuns source))
    (nelisp-aot-compile-sexp
     source (expand-file-name (concat name ".nl") codegen-runtime--out))))
