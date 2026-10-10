;;; nelisp-native-optimizer-bytecode.el --- Cold compiler bytecode -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;; GNU converts whole lexical initializers, including their private captures,
;; BEFORE the standalone loader authenticates and freezes the owner identities.
;; No validator or captured owner is replaced after cold preparation.
(require 'bytecomp)
(require 'nelisp-native-cache)
(require 'nelisp-prelude-bytecode)
(require 'nelisp-native-gccjit)
(require 'nelisp-bytecode-native-rooted-cfg-native)
(setq max-lisp-eval-depth (max 10000 max-lisp-eval-depth))
(defun nelisp-native-optimizer-bytecode--source (form)
  "Lower supported buffer macros before GNU lexical closure conversion."
 (cond
  ((not (consp form)) form)
  ((or (eq (car form) 'quote) (eq (car form) (intern "`"))) form)
  ((memq (car form) '(defun cl-defun defsubst defmacro cl-defmacro))
   (cons (car form) (cons (cadr form) (cons (caddr form) (mapcar #'nelisp-native-optimizer-bytecode--source (cdddr form))))))
  ((eq (car form) 'lambda) (cons 'lambda (cons (cadr form) (mapcar #'nelisp-native-optimizer-bytecode--source (cddr form)))))
  ((eq (car form) 'function) (list 'function (if (symbolp (cadr form)) (cadr form) (nelisp-native-optimizer-bytecode--source (cadr form)))))
  (t (let ((expanded (if (memq (car form) '(with-temp-buffer with-temp-file with-current-buffer)) (macroexpand form) form)))
    (cond
     ((not (equal form expanded)) (nelisp-native-optimizer-bytecode--source expanded))
     ((eq (car form) 'save-current-buffer)
      (let ((saved (make-symbol "p35-saved-buffer")))
       (nelisp-native-optimizer-bytecode--source `(let ((,saved (current-buffer)))
                      (unwind-protect (progn ,@(cdr form)) (set-buffer ,saved))))))
     ((memq (car form) '(let let*))
      (cons (car form)
       (cons (mapcar (lambda (binding)
                       (if (or (symbolp binding) (not (proper-list-p binding))) binding
                        (cons (car binding) (mapcar #'nelisp-native-optimizer-bytecode--source (cdr binding))))) (cadr form))
             (mapcar #'nelisp-native-optimizer-bytecode--source (cddr form)))))
     (t (if (proper-list-p form) (mapcar #'nelisp-native-optimizer-bytecode--source form)
          (cons (nelisp-native-optimizer-bytecode--source (car form)) (nelisp-native-optimizer-bytecode--source (cdr form))))))))))
(defun nelisp-native-optimizer-bytecode--compile (source)
 (let* ((lexical-binding (or lexical-binding (eq (type-of source) 'interpreted-function) (eq (car-safe source) 'closure)))
        ;; The VM's inline EQUAL compares vector identity; use the existing
        ;; Lisp structural comparator for compiler records. Unsupported inline
        ;; buffer, division and variable-arity instructions also use normal CALL.
        (names '(equal member - / concat fset goto-char insert point point-max point-min eobp current-buffer
                 set-buffer skip-chars-forward forward-line downcase string<))
        (properties (mapcar (lambda (name) (cons name (get name 'byte-compile))) names))
        (diagnostics nil)
        (logger byte-compile-log-warning-function)
        ;; GNU can return a bytecode object after reporting a fatal error,
        ;; notably when a recursive defsubst exhausts the inline depth limit.
        ;; Preserve its diagnostics, but never publish that partial object.
        (byte-compile-log-warning-function
         (lambda (message position &optional fill level)
           (when (eq level :error) (push message diagnostics))
           (funcall logger message position fill level))))
  (unwind-protect
   (progn
    (dolist (name names) (put name 'byte-compile nil))
    (let* ((lambda-source
            (cond ((eq (type-of source) 'interpreted-function)
                   (unless (equal (aref source 2) '(t)) (error "Captured ordinary function"))
                   (cons 'lambda (cons (aref source 0) (aref source 1))))
                  ((and (eq (car-safe source) 'closure) (equal (cadr source) '(t)))
                   (cons 'lambda (cddr source)))
                  (t source)))
           (lowered (nelisp-native-optimizer-bytecode--source lambda-source)))
     (let ((compiled (byte-compile (if (equal lowered lambda-source) source lowered))))
       (when diagnostics
         (error "Refusing diagnostic bytecode: %s" (car (last diagnostics))))
       compiled)))
   (dolist (entry properties) (put (car entry) 'byte-compile (cdr entry))))))
(defun nelisp-native-optimizer-bytecode--normalize (object seen)
 (or (gethash object seen)
  (cond
   ((eq (type-of object) 'interpreted-function)
    (unless (equal (aref object 2) '(t)) (error "Captured nested source function"))
    (let ((result (cons 'closure (cons '(t) (cons (aref object 0) (aref object 1))))))
     (puthash object result seen) result))
   ((byte-code-function-p object)
    (let ((slots (cl-loop for i below (length object) collect (aref object i))))
     ;; The fast reader type-checks code/constants before resolving #N#
     ;; labels. Keep these two header fields as concrete, unshared literals.
     (setcar (nthcdr 1 slots) (copy-sequence (aref object 1)))
     (setcar (nthcdr 2 slots) (copy-sequence (nelisp-native-optimizer-bytecode--normalize (aref object 2) seen)))
     (let ((result (apply #'make-byte-code slots))) (puthash object result seen) result)))
   ((vectorp object)
    (let ((result (copy-sequence object)))
     (puthash object result seen)
     (dotimes (i (length result)) (aset result i (nelisp-native-optimizer-bytecode--normalize (aref result i) seen))) result))
   ((consp object)
    (let ((result (cons nil nil)))
     (puthash object result seen)
     (setcar result (nelisp-native-optimizer-bytecode--normalize (car object) seen))
     (setcdr result (nelisp-native-optimizer-bytecode--normalize (cdr object) seen)) result))
   ((hash-table-p object)
    (let ((result (make-hash-table :test (hash-table-test object))))
     (puthash object result seen)
     (maphash (lambda (k v) (puthash (nelisp-native-optimizer-bytecode--normalize k seen) (nelisp-native-optimizer-bytecode--normalize v seen) result)) object) result))
   (t object))))
(defun nelisp-native-optimizer-bytecode--project-form (form)
  "Derive FORM on the build host before trusted owner publication.
Return (replacement selected unsupported); unsupported forms retain source."
  (let ((selected (memq (car-safe form) '(let let* defun cl-defun defsubst)))
        (result form) bad)
    (when selected
      (let* ((candidate `(lambda () ,form))
             (compiled (let ((lexical-binding t))
                         (nelisp-native-optimizer-bytecode--normalize
                          (nelisp-native-optimizer-bytecode--compile candidate)
                          (make-hash-table :test 'eq))))
             (ops (and (byte-code-function-p compiled)
                       (nelisp-prelude-bytecode--decode-opcodes (aref compiled 1)))))
        (setq bad (or (null ops)
                      (cl-set-difference ops nelisp-prelude-bytecode--opcodes)
                      (nelisp-prelude-bytecode--nested-unsupported compiled)))
        (unless bad (setq result (list 'funcall compiled)))))
    (list result selected bad)))

(let* ((root (expand-file-name ".." (file-name-directory load-file-name)))
       (directory (expand-file-name "target/nelisp-compiler-bytecode" root))
       (modules (delq 'nelisp-native-cache (copy-sequence nelisp-native-cache--compiler-modules)))
       (count 0) (skipped nil) (manifest nil))
 (make-directory directory t)
 (dolist (module modules) (require module))
 (dolist (module modules)
  (let ((source (locate-library (concat (symbol-name module) ".el") t)) forms)
   (with-temp-buffer
    (emacs-lisp-mode) (insert-file-contents source) (goto-char (point-min))
    (while (progn (forward-comment (point-max)) (< (point) (point-max)))
     (push (read (current-buffer)) forms)))
   (let ((output (expand-file-name (concat (symbol-name module) ".el") directory)))
   (with-temp-file output
    (insert ";;; GNU compiler bytecode projection -*- lexical-binding: t; -*-\n")
    (dolist (form (nreverse forms))
     (let* ((projection (nelisp-native-optimizer-bytecode--project-form form))
            (result (nth 0 projection))
            (selected (nth 1 projection))
            (bad (nth 2 projection)))
       (when selected
         (if bad
             (push (list module (car form) (and (symbolp (cadr form)) (cadr form)) bad) skipped)
           (setq count (1+ count))))
      (let ((print-length nil) (print-level nil) (print-gensym t) (print-circle t)
            (print-escape-newlines t) (print-escape-nonascii t))
       (prin1 (list 'let (list (list 'load-file-name source)) result) (current-buffer)) (insert "\n")))))
   (push (list :module module :source (file-relative-name source root)
               :source-sha256 (with-temp-buffer (set-buffer-multibyte nil) (insert-file-contents-literally source) (secure-hash 'sha256 (current-buffer)))
               :output (file-relative-name output root)
               :output-sha256 (with-temp-buffer (set-buffer-multibyte nil) (insert-file-contents-literally output) (secure-hash 'sha256 (current-buffer)))) manifest))))
 (with-temp-file (expand-file-name "target/nelisp-compiler-bytecode-load.el" root)
  (insert ";;; Fresh compiler projection before owner capture -*- lexical-binding: t; -*-\n")
  (prin1 `(progn
            (when nelisp-native-cache--cold-source-check (error "Compiler already sealed"))
            (unless (featurep 'nelisp-native-structural-bytecode)
              (load ,(expand-file-name "target/nelisp-structural-bytecode.el" root) nil t t))
            (add-to-list 'load-path ,directory)
            (require 'nelisp-bytecode-native-rooted-cfg-shared-emit)
            (require 'nelisp-native-load)
            (nelisp-native-cache-prepare-cold-compiler)
            (setq load-path (delete ,directory load-path))) (current-buffer)))
 (with-temp-file (expand-file-name "target/nelisp-compiler-bytecode-manifest.el" root)
  (let ((print-length nil) (print-level nil) (print-gensym t) (print-circle t))
   (prin1 (list :format 1 :gnu-version emacs-version :compiled count :source-fallbacks (nreverse skipped) :modules (nreverse manifest)) (current-buffer)) (insert "\n")))
 (message "Compiler bytecode: %d initializers, source fallbacks: %S" count skipped))

(defvar nelisp-native-optimizer-bytecode--structural-source nil)
(let* ((root (expand-file-name ".." (file-name-directory load-file-name)))
       (structural-parts nil)
       (output (expand-file-name "target/nelisp-optimizer-bytecode.el" root)))
 (with-temp-file (expand-file-name "target/nelisp-structural-bytecode.el" root)
   (insert ";;; GNU bytecode of genuine structural equality -*- lexical-binding: nil; -*-\n"))
 (with-temp-file output
  (insert ";;; Generated genuine prelude helpers -*- lexical-binding: nil; -*-\n")
    ;; Read genuine prelude definitions without executing the prelude on GNU.
    ;; The printer's per-character Lisp calls dominate contract serialization.
    ;; The stat wrapper is shared by both interpreted and native leaf callers;
    ;; execute its unchanged Lisp body through the same general bytecode VM.
    (let (definitions)
      (cl-labels ((walk (form)
                    (when (consp form)
                      (cond
                       ((and (eq (car form) 'defun)
                             (or (string-prefix-p "nelisp--prn-" (symbol-name (cadr form)))
                                 (memq (cadr form) '(equal nelisp--equal-recursive copy-sequence cl-some cl-every hash-table-p functionp interpreted-function-p proper-list-p plist-put mapc reverse vconcat nelisp--check-seq-list nelisp-bytecode-jit--runtime-dispatch))))
                        (push (cons (cadr form) (cons 'lambda (cddr form))) definitions))
                       ((and (eq (car form) 'fset) (equal (cadr form) '(quote nelisp--syscall-stat))
                             (eq (car-safe (caddr form)) 'lambda))
                        (push (cons 'nelisp--syscall-stat (caddr form)) definitions))
                       ((memq (car form) '(progn when unless if))
                        (mapc #'walk (cdr form)))))))
        (with-temp-buffer
          (insert-file-contents (expand-file-name "scripts/nelisp-stdlib-prelude.el" root))
          (goto-char (point-min))
          (condition-case nil (while t (walk (read (current-buffer)))) (end-of-file nil))))
      ;; The same genuine GNU definition is embedded by the bootstrap loader.
      ;; Read only COPY-TREE, never execute the vendored library on the host.
      (with-temp-buffer
        (insert-file-contents (expand-file-name "vendor/staged-emacs-lisp/subr.el" root))
        (goto-char (point-min))
        (let ((found 0))
          (condition-case nil
              (while t
                (let ((form (read (current-buffer))))
                  (when (and (eq (car-safe form) 'defun) (eq (cadr form) 'copy-tree))
                    (setq found (1+ found))
                    (push (cons 'copy-tree (cons 'lambda (cddr form))) definitions))))
            (end-of-file nil))
          (unless (= found 1) (error "Expected one genuine COPY-TREE definition"))))
      ;; Preserve the source headers: prelude is dynamically scoped; the
      ;; vendored COPY-TREE alone is lexical. Callbacks may observe bindings.
      (dolist (definition (nreverse definitions))
        (let* ((name (car definition))
               (compiled (let ((lexical-binding (eq name 'copy-tree))) (nelisp-native-optimizer-bytecode--compile (cdr definition))))
               (print-length nil) (print-level nil) (print-gensym t) (print-circle t) (print-escape-nonascii t))
          (unless (byte-code-function-p compiled) (error "Prelude helper bytecode refused: %s" name))
          (let ((ops (nelisp-prelude-bytecode--decode-opcodes (aref compiled 1))))
            (unless (and ops (not (cl-set-difference ops nelisp-prelude-bytecode--opcodes))
                         (not (nelisp-prelude-bytecode--nested-unsupported compiled)))
              (error "Prelude helper uses unsupported bytecode: %s" name)))
          (if (memq name '(equal nelisp--equal-recursive copy-sequence cl-some cl-every hash-table-p functionp interpreted-function-p proper-list-p plist-put mapc reverse vconcat nelisp--check-seq-list nelisp-bytecode-jit--runtime-dispatch copy-tree))
              (with-temp-buffer
                (let ((print-length nil) (print-level nil) (print-gensym t) (print-circle t) (print-escape-newlines t))
                  (prin1 `(when (and (fboundp ',name)
                              (fboundp 'nelisp--native-functionp))
                     (fset ',name ,compiled)) (current-buffer))
                  (insert "\n"))
                (push (buffer-string) structural-parts)
                (write-region (point-min) (point-max)
                              (expand-file-name "target/nelisp-structural-bytecode.el" root) t))
            (prin1 `(when (fboundp ',name) (fset ',name ,compiled)) (current-buffer))
            (insert "\n")))))
    (setq nelisp-native-optimizer-bytecode--structural-source
          (concat ";;; GNU bytecode of genuine structural equality -*- lexical-binding: nil; -*-\n"
                  (apply #'concat (nreverse structural-parts))
                  "(provide 'nelisp-native-structural-bytecode)\n"))
    (with-temp-buffer
      (insert "(provide 'nelisp-native-structural-bytecode)\n")
      (write-region (point-min) (point-max)
                    (expand-file-name "target/nelisp-structural-bytecode.el" root) t))
    (insert "t\n")))

(provide 'nelisp-native-optimizer-bytecode)
