;;; compile.el --- compile pinned runtime fixtures -*- lexical-binding: t; -*-
(require 'json)
(require 'nelisp-standalone-build)
(defvar nelisp-aot-compiler--vector-value-convention)
(defvar nelisp-aot-compiler--x86-inline-data-hook)
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
  (when (and (not (getenv "CODEGEN_RUNTIME_VECTOR_CONVENTION"))
             (not (boundp 'nelisp-aot-compiler--vector-value-convention)))
    (error "Compiler compact-word vector convention is not available"))
  (let* ((nelisp-aot-compiler--vector-value-convention
          (intern (or (getenv "CODEGEN_RUNTIME_VECTOR_CONVENTION") "compact-word")))
         (nelisp-standalone--target 'linux-x86_64)
         (source (cons 'seq (codegen-runtime--forms "runtime.nl.el")))
         (inline-data nil)
         (nelisp-aot-compiler--x86-inline-data-hook
          (list (lambda (_buffer offset size) (push (list offset size) inline-data))))
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
    ;; Exact emitter-reported extents, relative to the emitted text buffer.
    (with-temp-file (expand-file-name "runtime.nl.inline-data.json" codegen-runtime--out)
      (insert (json-encode (vconcat (delete-dups inline-data)))))
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
                                   (append (codegen-runtime--forms "runtime.nl.el")
                                           (when (file-exists-p (expand-file-name "dependencies.nl.el" codegen-runtime--directory))
                                             (codegen-runtime--forms "dependencies.nl.el"))
                                           (when (file-exists-p (expand-file-name "heap-fields.nl.el" codegen-runtime--directory))
                                             (codegen-runtime--forms "heap-fields.nl.el")))))
         (extra (append (codegen-runtime--forms (concat name ".nl.el"))
                        (when (cdr (assq 'auto_cold (cdr entry)))
                          (codegen-runtime--forms
                           (expand-file-name (concat name ".cold.nl.el") codegen-runtime--out)))))
         (harness (codegen-runtime--forms "harness.nl.el")))
    (setq source (cl-delete-duplicates source :key #'cadr :test #'eq :from-end t))
    ;; Identical within-unit small-defun inlining to the runtime build.
    ;; AOT's executable frontend supplies _start and OS entry handling.
    (let ((addresses (cdr (assq 'data_addresses (cdr entry)))))
      (cl-labels ((rewrite (x)
                    (cond ((not (consp x)) x)
                          ((and (eq (car x) 'data-addr) (assq (cadr x) addresses))
                           (cdr (assq (cadr x) addresses)))
                          (t (mapcar #'rewrite x)))))
        (setq source (rewrite (cons 'seq (append source harness extra '((exit (cr_start)))))))))
    (let* ((mode (intern (or (getenv "CODEGEN_RUNTIME_VECTOR_CONVENTION")
                             (cdr (assq 'vector_convention (cdr entry))) "legacy-view")))
           (available (boundp 'nelisp-aot-compiler--vector-value-convention)))
      (when (and (eq mode 'compact-word) (not available))
        (error "Compiler compact-word vector convention is not available"))
      ;; Match standalone raw-unit production policy; API default is unchanged.
      (let ((nelisp-aot-compiler--vector-value-convention mode)
            (nelisp-aot-compiler-tco-enabled
             (not (equal (getenv "NELISP_TCO") "0"))))
    (if (cdr (assq 'unit_frontend (cdr entry)))
        (let* ((nelisp-standalone--target 'linux-x86_64)
               (body (cons 'seq (append (butlast (cdr source))
                                        '((defun _start ()
                                            (seq
                                             (ptr-write-u64 (data-addr nl_arena_base) 0 (shl 1 28))
                                             (syscall-direct 60 (cr_start) 0 0 0 0 0)))))))
               (unit (nelisp-standalone--compile-to-unit "runtime-fixture" body)))
          (nelisp-link-units (expand-file-name (concat name ".nl") codegen-runtime--out)
                             (list unit (nelisp-standalone--arena-base-slot-unit)) "_start"))
      (setq source (nelisp-standalone--inline-unit-source source))
      (nelisp-aot-compile-sexp
       source (expand-file-name (concat name ".nl") codegen-runtime--out)))))))
