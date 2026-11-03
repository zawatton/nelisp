;;; wasm-genuine-alloc.el --- Opt-in allocator composition -*- lexical-binding: t; -*-
;; Same trust boundary as wasm-target-alloc.el, including reviewed harness code.
(require 'wasm-target-alloc)
(defconst wasm-genuine-alloc-consumer-names
  '(nl_alloc_consbox nl_alloc_consbox_init nl_alloc_vector nl_alloc_vector_build
    nl_alloc_vector_fill nl_alloc_vector_with_data))
(defun wasm-genuine-alloc-consumers ()
  "Read selected genuine consumers in a bounded isolated child."
  (with-temp-buffer
    (let* ((root wasm-rootstack--root)
           (expression
            '(progn
               (require 'nelisp-cc-nlconsbox-alloc)
               (require 'nelisp-cc-nlvector-alloc)
               (princ "CONSUMERS=")
               (prin1 (append (cdr nelisp-cc-nlconsbox-alloc--source)
                              (cdr nelisp-cc-nlvector-alloc--source)))))
           (rc (call-process "bash" nil t nil "-c" "ulimit -v 8388608; exec \"$@\"" "consumers"
                             "timeout" "120" (expand-file-name invocation-name invocation-directory)
                             "-Q" "--batch" "--eval" "(setq load-no-native t)"
                             "-L" (expand-file-name "lisp" root)
                             "--eval" (prin1-to-string expression))))
      (unless (equal rc 0) (error "Consumer oracle rc=%s %s" rc (buffer-string)))
      (goto-char (point-min))
      (unless (search-forward "CONSUMERS=" nil t) (error "Missing consumer record"))
      (let ((definitions (sort (read (current-buffer))
                               (lambda (a b) (string< (symbol-name (cadr a)) (symbol-name (cadr b)))))))
        (unless (equal (mapcar #'cadr definitions) wasm-genuine-alloc-consumer-names)
          (error "Consumer closure membership"))
        definitions))))

(defun wasm-genuine-alloc-validate (path contract owners closure consumers)
  "Extend the independent allocator schema with exactly the genuine CONSUMERS."
  (let* ((source (with-temp-buffer
                   (insert-file-contents path)
                   (prog1 (read (current-buffer))
                     (skip-chars-forward " \t\r\n")
                     (unless (eobp) (error "Trailing consumer input")))))
         (base (list 'seq)) seen
         (scratch (concat path ".base.eld")))
    (unless (and (proper-list-p source) (eq (car-safe source) 'seq))
      (error "Unexpected consumer input: expected seq"))
    (dolist (form (cdr source))
      (if (memq (cadr form) wasm-genuine-alloc-consumer-names)
          (progn
            (unless (and (not (memq (cadr form) seen))
                         (equal form (cl-find (cadr form) consumers :key #'cadr)))
              (error "Unexpected consumer definition %s" (cadr form)))
            (push (cadr form) seen))
        (setq base (append base (list form)))))
    (unless (= (length seen) (length wasm-genuine-alloc-consumer-names))
      (error "Missing consumer definition"))
    (unwind-protect
        (progn
          (with-temp-file scratch (let ((print-length nil) (print-level nil)) (prin1 base (current-buffer))))
          (wasm-target-alloc-validate-compiler-input scratch contract owners closure wasm-target-alloc-adapters))
      (when (file-exists-p scratch) (delete-file scratch)))
    source))

(defun wasm-genuine-alloc-compile-file (input output mode)
  "Compile saved INPUT in a clean child; return its hashed provenance.
The caller validates the schema first.  The parent enforces the named compiler
file allowlist and the runtime-derived Emacs installation root on recorded loads.
A prelaunch name check and appended load path prevent installation shadowing."
  (let* ((root wasm-rootstack--root)
         (process-environment (copy-sequence process-environment))
         ;; This expression is passed as data to -Q, never loaded as a helper.
         (expression
          `(let ((input ,(expand-file-name input)) (output ,(expand-file-name output))
                 (nelisp-aot-compiler-wasm-genuine-alloc ,mode)
                 (nelisp-aot-compiler-wasm-genuine-alloc-raw-reservers '(nl_wasm_arena_init))
                 diagnostic)
             (condition-case err
                 (let ((source (with-temp-buffer
                                 (insert-file-contents input) (read (current-buffer)))))
                   (nelisp-aot-compile-to-object source output :arch 'wasm32 :format 'wasm))
               (error (setq diagnostic err)))
             (let* ((files (sort (delete-dups (delq nil (mapcar #'car load-history))) #'string<))
                    (hash-file (lambda (file)
                                 (with-temp-buffer
                                   (set-buffer-multibyte nil)
                                   (insert-file-contents-literally file)
                                   (secure-hash 'sha256 (current-buffer)))))
                    (print-length nil) (print-level nil) (print-circle nil))
               ;; Native redirects can report an .elc row for executed .eln
               ;; code; load-no-native must precede all compiler loads. Also
               ;; reject any explicit native row before publishing provenance.
               (dolist (file files)
                 (when (string-match-p "\\.eln\\'" file)
                   (error "Native compiler child load forbidden %s" file)))
               (princ "WASM-TARGET-ALLOC-COMPILE=")
               (prin1 (list :genuine-alloc nelisp-aot-compiler-wasm-genuine-alloc
                            :raw-reservers nelisp-aot-compiler-wasm-genuine-alloc-raw-reservers
                            :diagnostic diagnostic
                            :installation-root
                            (file-name-as-directory
                             (file-truename (if (boundp 'lisp-directory) lisp-directory
                                              (expand-file-name "../lisp" data-directory))))
                            :input-sha256 (funcall hash-file input)
                            :wasm-sha256 (unless diagnostic (funcall hash-file output))
                            :files (mapcar (lambda (file) (cons file (funcall hash-file file))) files)))
               (terpri))
             (kill-emacs (if diagnostic 1 0)))))
    (dolist (file (list output (concat output ".compile.eld")))
      (when (file-exists-p file) (delete-file file)))
    (setenv "EMACSLOADPATH" nil)
    (setenv "EMACSNATIVELOADPATH" nil)
    (condition-case err
        (progn
          (wasm-target-alloc-check-library-shadows)
          (with-temp-buffer
            (let ((rc (apply #'call-process "bash" nil t nil
                             (append
                              (list "-c" "ulimit -v 8388608; exec \"$@\"" "compiler" "timeout" "120"
                                    (expand-file-name invocation-name invocation-directory) "-Q" "--batch")
                              (list "--eval" "(setq load-no-native t)")
                              (list "--eval" (prin1-to-string
                                              `(add-to-list 'load-path ,(expand-file-name "lisp" root) t)))
                              (list "--eval" "(setq load-prefer-newer t)"
                                    "-l" (expand-file-name "lisp/nelisp-asm-wasm.el" root)
                                    "-l" (expand-file-name "lisp/nelisp-aot-compiler.el" root)
                                    "--eval" (prin1-to-string expression))))))
              (goto-char (point-min))
              (unless (re-search-forward "^WASM-TARGET-ALLOC-COMPILE=" nil t)
                (error "Allocator compiler rc=%s missing record: %s" rc (buffer-string)))
              (let* ((record (read (current-buffer))) (diagnostic (plist-get record :diagnostic)))
                (unless (equal rc (if diagnostic 1 0))
                  (error "Allocator compiler rc=%s inconsistent record" rc))
                (unless (and (eq (plist-get record :genuine-alloc) mode)
                             (equal (plist-get record :raw-reservers) '(nl_wasm_arena_init)))
                  (error "Compiler child allocator mode mismatch"))
                (wasm-target-alloc-compiler-files (plist-get record :files)
                                                  (plist-get record :installation-root) diagnostic)
                (unless (equal (plist-get record :input-sha256) (wasm-rootstack--hash input))
                  (error "Allocator compiler input identity mismatch"))
                (when diagnostic (signal (car diagnostic) (cdr diagnostic)))
                (unless (equal (plist-get record :wasm-sha256) (wasm-rootstack--hash output))
                  (error "Allocator compiler module identity mismatch"))
                (with-temp-file (concat output ".compile.eld") (prin1 record (current-buffer)))
                record))))
      (error
       ;; A rejected child may already have emitted a module. Never publish it.
       (when (file-exists-p output) (delete-file output))
       (signal (car err) (cdr err))))))


(provide 'wasm-genuine-alloc)
