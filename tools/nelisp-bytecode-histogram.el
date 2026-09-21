;;; nelisp-bytecode-histogram.el --- Source bytecode census -*- lexical-binding: t; -*-

;; Run via nelisp-bytecode-histogram.sh.  Source files are read, never compiled
;; in place.  Each source defun/defsubst is compiled anew with its lexical mode.
;; Nested bytecode constants count toward their enclosing source function.

(require 'cl-lib)
(require 'nelisp-bytecode-decode)

(defconst nelisp-bytecode-source-files
  '("emacs-lisp/cl-seq.el.gz" "emacs-lisp/rx.el.gz"
    "emacs-lisp/subr-x.el.gz" "calendar/time-date.el.gz"
    "json.el.gz" "subr.el.gz")
  "Source paths relative to the running Emacs Lisp directory.")

(defun nelisp-bytecode-source-paths ()
  "Locate the default corpus using the running Emacs installation."
  (let ((root (file-name-directory (locate-library "subr"))))
    (mapcar (lambda (file) (expand-file-name file root))
            nelisp-bytecode-source-files)))

(defun nelisp-bytecode-compile-sources (files)
  "Return (FILE NAME OBJECT) entries for every source definition in FILES.
Compile defun and defsubst forms, including ones inside top-level wrappers.
Load libraries first to make their macros and special variables available.
Read quoted data and function bodies without treating them as definitions."
  (dolist (file files)
    (load (file-name-sans-extension (file-name-sans-extension file)) nil t))
  (let (result)
    (dolist (file files)
      (with-temp-buffer
        (insert-file-contents file)
        (set-syntax-table emacs-lisp-mode-syntax-table)
        (goto-char (point-min))
        (let ((lexical-binding
               (save-excursion
                 (re-search-forward "lexical-binding: *t" (line-end-position) t)))
              (byte-compile-warnings nil))
          (cl-labels
              ((collect
                (form)
                (when (consp form)
                  (cond
                   ((memq (car form) '(defun defsubst))
                    (let* ((name (cadr form))
                           ;; Defun consumes declarations before compiling its
                           ;; lambda; retain docstrings and interactive specs.
                           (body (cl-remove-if
                                  (lambda (item)
                                    (eq (car-safe item) 'declare))
                                  (cdddr form)))
                           (lambda-form (cons 'lambda
                                              (cons (nth 2 form) body)))
                           (object (byte-compile
                                    (eval (list 'function lambda-form)
                                          lexical-binding))))
                      (unless (byte-code-function-p object)
                        (error "Did not compile %s in %s" name file))
                      (push (list file name object) result)))
                   ((memq (car form) '(quote function defmacro cl-defmacro)))
                   (t (mapc #'collect form))))))
            (while (progn (forward-comment (point-max)) (not (eobp)))
              (collect (read (current-buffer))))))))
    (nreverse result)))

(defun nelisp-bytecode-objects (object)
  "Return OBJECT and all its nested bytecode constants, once by identity."
  (let ((seen (make-hash-table :test #'eq)) result)
    (cl-labels ((visit (value)
                 (when (and (byte-code-function-p value)
                            (not (gethash value seen)))
                   (puthash value t seen)
                   (push value result)
                   (mapc #'visit (aref value 2)))))
      (visit object))
    (nreverse result)))

(defun nelisp-bytecode-write-histogram (files output)
  "Compile FILES and write the opcode census to OUTPUT."
  (let ((counts (make-hash-table :test #'eq))
        (functions (make-hash-table :test #'eq))
        (entries (nelisp-bytecode-compile-sources files))
        rows)
    (dolist (entry entries)
      (let ((seen (make-hash-table :test #'eq)))
        (dolist (object (nelisp-bytecode-objects (nth 2 entry)))
          (dolist (instruction (nelisp-bytecode-decode (aref object 1)))
            (let ((op (nth 1 instruction)))
              (puthash op (1+ (gethash op counts 0)) counts)
              (puthash op t seen))))
        (maphash (lambda (op _)
                   (puthash op (1+ (gethash op functions 0)) functions))
                 seen)))
    (maphash (lambda (op count)
               (push (list op count (gethash op functions)) rows))
             counts)
    (setq rows (sort rows (lambda (a b)
                           (if (= (cadr a) (cadr b))
                               (string< (symbol-name (car a))
                                        (symbol-name (car b)))
                             (> (cadr a) (cadr b))))))
    (unless rows (error "Empty bytecode corpus"))
    (with-temp-file output
      (insert (format "# Date: %s\n# Emacs: %s\n"
                      (format-time-string "%Y-%m-%d %z") emacs-version))
      (insert "# Recipe: bash tools/nelisp-bytecode-histogram.sh\n")
      (dolist (file files)
        (insert (format "# File: %s\n"
                        (file-relative-name file
                                            (file-name-directory
                                             (locate-library "subr"))))))
      (insert (format "# Source definitions: %d\n" (length entries)))
      (insert "# Nested bytecode counts toward its enclosing definition.\n")
      (insert "# Names normalized as in disassemble; only observed opcodes.\n")
      (insert "# OPCODE-NAME\tOCCURRENCES\tFUNCTIONS-CONTAINING-IT\n")
      (dolist (row rows)
        (insert (format "%s\t%d\t%d\n" (car row) (cadr row) (caddr row)))))
    (message "Measured %d definitions; wrote %s" (length entries) output)))

(provide 'nelisp-bytecode-histogram)
;;; nelisp-bytecode-histogram.el ends here
