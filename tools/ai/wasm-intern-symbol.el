;;; wasm-intern-symbol.el --- Genuine interning composition -*- lexical-binding: t; -*-
;; trusted compiler inputs are the running Emacs installation lisp directory and the eight named repository compiler files, hash-checked. Harness sources under tools/ai are trusted by review (the lane diff gate); attacks that require the harness itself to write files or tamper with process state at run time are out of scope. The checker defends against drift in genuine sources and artifacts, manifest inconsistency, and accidental environment contamination (session definitions, load-path shadowing).
(require 'wasm-target-alloc)
(defconst wasm-intern-symbol-directory "target/wasm-intern-symbol/")
(defconst wasm-intern-symbol-names
  '(nl_alloc_str_copy_loop nl_alloc_symbol nl_alloc_symbol_plain nl_alloc_symbol_pos
    nl_alloc_symbol_write nl_intern_arena_fits nl_intern_bump nl_intern_eq
    nl_intern_finish nl_intern_hash nl_intern_insert nl_intern_probe
    nl_intern_slotmatch nl_intern_write_sexp nl_name_is_nil nl_name_is_t
    nl_write_canonical_nil nl_write_canonical_t))
(defvar wasm-intern-symbol--record nil)
(defun wasm-intern-symbol-shadow-check ()
  "Check every oracle search directory before loading repository libraries."
  (let ((libraries (make-hash-table :test #'equal)))
    (dolist (file (directory-files-recursively (wasm-target-alloc-emacs-lisp-root)
                                               "\\.elc?\\(?:\\.gz\\)?\\'"))
      (puthash (replace-regexp-in-string "\\.elc?\\(?:\\.gz\\)?\\'" ""
                                            (file-name-nondirectory file)) t libraries))
    (dolist (dir '("lisp" "src" "scripts"))
      (dolist (file (directory-files (expand-file-name dir wasm-rootstack--root) nil "\\.elc?\\'"))
        (when (gethash (file-name-sans-extension file) libraries)
          (error "Repository library shadows installation library %s" file))))))
(defmacro wasm-intern-symbol-isolated (&rest body)
  "Use the existing oracles with clean, appended repository search paths."
  (declare (indent 0))
  `(let ((original (symbol-function 'call-process))
         (process-environment (copy-sequence process-environment)))
     (wasm-intern-symbol-shadow-check)
     (setenv "EMACSLOADPATH" nil) (setenv "EMACSNATIVELOADPATH" nil)
     (setenv "NELISP_RUNTIME_RELOAD" nil)
     (cl-letf (((symbol-function 'call-process)
                (lambda (program &optional infile destination display &rest arguments)
                  (let (clean)
                    (while arguments
                      (let ((arg (pop arguments)))
                        (if (equal arg "-L")
                            (setq clean (append clean (list "--eval"
                              (prin1-to-string `(add-to-list 'load-path ,(pop arguments) t)))))
                          (setq clean (append clean (list arg)))
                          (when (equal arg "--batch")
                            (setq clean (append clean '("--eval" "(setq load-no-native t)")))))))
                    (apply original program infile destination display clean)))))
       ,@body)))
(defun wasm-intern-symbol-record ()
  "Extract the genuine source closure and execute the native hash oracle."
  (or wasm-intern-symbol--record
      (setq wasm-intern-symbol--record
        (with-temp-buffer
          (let* ((helper (expand-file-name "tools/ai/wasm-intern-symbol-oracle.el" wasm-rootstack--root))
                 (rc (call-process "bash" nil t nil "-c" "ulimit -v 8388608; exec \"$@\"" "oracle"
                     "timeout" "180" (expand-file-name invocation-name invocation-directory)
                     "-Q" "--batch" "--eval" "(setq load-no-native t load-prefer-newer t)"
                     "--eval" (prin1-to-string `(progn ,@(mapcar
                       (lambda (dir) `(add-to-list 'load-path ,(expand-file-name dir wasm-rootstack--root) t))
                       '("lisp" "src" "scripts" "tools/ai"))))
                     "-l" helper "-f" "wasm-intern-symbol-oracle")))
            (unless (equal rc 0) (error "Intern oracle rc=%s %s" rc (buffer-string))))
          (goto-char (point-min))
          (unless (re-search-forward "^WASM-INTERN-SYMBOL=" nil t) (error "Missing intern oracle record"))
          (let ((record (read (current-buffer))))
            (unless (and (equal (mapcar #'cadr (plist-get record :definitions)) wasm-intern-symbol-names)
                         (wasm-rootstack--oracle-files-current-p (plist-get record :files)))
              (error "Intern oracle membership or identity mismatch"))
            record)))))
(defun wasm-intern-symbol-derive (definition)
  "Derive the W0 adapter from the exact genuine mmap syntax, without evaluation."
  (pcase definition
    (`(defun nl_intern_region_init nil
        (let ((rbase (syscall-direct 9 0 ,total 3 34 -1 0)))
          (if (< rbase ,page) 0
            (seq (ptr-write-u64 ,table-atom 0 rbase)
                 (ptr-write-u64 ,cursor-atom 0 (+ rbase ,table-size))))))
     (let* ((origin (nelisp-standalone-arena-rewrite-base))
            (span (nelisp-standalone-arena-rewrite-span)))
       (unless (and (cl-every #'integerp (list total page table-atom cursor-atom table-size))
                    (= page 4096) (> total table-size) (> table-size 0)
                    (= (% total page) 0) (= (% table-size page) 0)
                    (<= origin table-atom) (< cursor-atom (+ origin span))
                    (= cursor-atom (+ table-atom 8)) (= (% table-atom 8) 0))
         (error "Intern adapter derivation: invalid mmap layout"))
       (list :total total :page page :table-size table-size
             :table-offset (- table-atom origin) :cursor-offset (- cursor-atom origin)
             :adapter
             (wasm-target-init-rewrite
              `(defun nl_wasm_intern_region_init ()
                 ;; Raw reservation uses mmap page alignment, 4096 bytes.
                 (let ((rbase (alloc-bytes ,total ,page)))
                   (if (< rbase ,page) 0
                     (seq (ptr-write-u64 ,table-atom 0 rbase)
                          (ptr-write-u64 ,cursor-atom 0 (+ rbase ,table-size))))))))))
    (_ (error "Intern adapter derivation: genuine init syntax mismatch"))))
(defun wasm-intern-symbol-extras (derived)
  "Fixed harness storage and module-derived range exports."
  `((data-blob wasm_intern_input ,(make-string 64 0) bss)
    (data-blob wasm_intern_result ,(make-string 32 0) bss)
    (defun address-intern-input () (data-addr wasm_intern_input))
    (defun address-intern-result () (data-addr wasm_intern_result))
    (defun intern-input-size () 64)
    (defun intern-result-size () 32)
    (defun intern-region-size () ,(plist-get derived :total))
    (defun intern-page-size () ,(plist-get derived :page))
    (defun intern-table-size () ,(plist-get derived :table-size))
    (defun intern-table-offset () ,(plist-get derived :table-offset))
    (defun intern-cursor-offset () ,(plist-get derived :cursor-offset))))
(defun wasm-intern-symbol-region-contract (init definitions)
  "Bind the reservation to the genuine consumers' extent and probe syntax."
  (let* ((derived (wasm-intern-symbol-derive init))
         (origin (nelisp-standalone-arena-rewrite-base))
         (table-atom (+ origin (plist-get derived :table-offset)))
         (cursor-atom (+ origin (plist-get derived :cursor-offset)))
         (fits (cl-find 'nl_intern_arena_fits definitions :key #'cadr))
         (pos (cl-find 'nl_alloc_symbol_pos definitions :key #'cadr))
         (probe (cl-find 'nl_intern_probe definitions :key #'cadr))
         (initial-masks (wasm-rootstack--forms pos
                         (lambda (f) (and (eq (car f) 'logand)
                                          (eq (car-safe (cadr f)) 'nl_intern_hash)))))
         (probe-masks (wasm-rootstack--forms probe
                       (lambda (f) (equal (seq-take f 2) '(logand (+ idx 1))))))
         (strides (wasm-rootstack--forms probe
                    (lambda (f) (equal (seq-take f 2) '(* idx))))))
    (pcase fits
      (`(defun nl_intern_arena_fits (n)
          (if (<= (+ (ptr-read-u64 ,cursor 0) (if (= n 0) 1 n))
                  (+ (ptr-read-u64 ,table 0) ,limit)) 1 0))
       (unless (and (equal cursor cursor-atom) (equal table table-atom)
                    (equal limit (plist-get derived :total))
                    ;; This slice intentionally retains the genuine fixed extent.
                    (equal limit 67108864))
         (error "Intern region extent mismatch: init=%s consumer=%s"
                (plist-get derived :total) limit)))
      (_ (error "Intern region consumer syntax mismatch: nl_intern_arena_fits")))
    (let ((mask (nth 2 (car initial-masks))))
      (unless (and (= (length initial-masks) 1) (= (length probe-masks) 1)
                   (integerp mask) (= mask 1048575)
                   (equal (nth 2 (car probe-masks)) mask)
                   (equal (nth 1 (nth 3 probe)) `(< ,mask k))
                   strides (cl-every (lambda (f) (equal f '(* idx 16))) strides)
                   (= (plist-get derived :table-size) (* (1+ mask) 16)))
        (error "Intern table extent mismatch: init=%s masks=%S/%S strides=%S"
               (plist-get derived :table-size) initial-masks probe-masks strides)))
    derived))
(defun wasm-intern-symbol-validate (path contract owners closure record &optional omitted)
  "Validate an independently closed extension of the allocator schema."
  (let* ((source (with-temp-buffer (insert-file-contents path)
                   (prog1 (read (current-buffer)) (skip-chars-forward " \t\r\n")
                     (unless (eobp) (error "Trailing intern input")))))
         (derived (wasm-intern-symbol-region-contract (plist-get record :init)
                                                     (plist-get record :definitions)))
         (expected (append (wasm-target-init-rewrite (plist-get record :definitions))
                           (list (if omitted (wasm-target-init-rewrite (plist-get record :init))
                                   (plist-get derived :adapter)))
                           (wasm-intern-symbol-extras derived)))
         (base '(seq)) seen
         (scratch (concat path ".base.eld")))
    (unless (and (proper-list-p source) (eq (car source) 'seq)) (error "Intern schema: expected seq"))
    (dolist (form (cdr source))
      (let ((row (cl-find (cadr form) expected :key #'cadr)))
        (if row
            (progn (unless (and (not (memq (cadr form) seen)) (equal row form))
                     (error "Intern schema: unexpected definition %s" (cadr form)))
                   (push (cadr form) seen))
          (setq base (append base (list form))))))
    (unless (= (length seen) (length expected)) (error "Intern schema: missing definition"))
    (unwind-protect
        (progn (with-temp-file scratch (let ((print-length nil) (print-level nil)) (prin1 base (current-buffer))))
               (wasm-target-alloc-validate-compiler-input scratch contract owners closure wasm-target-alloc-adapters))
      (when (file-exists-p scratch) (delete-file scratch)))
    source))
(defun wasm-intern-symbol-compile-file (input output mode)
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
                 (nelisp-aot-compiler-wasm-genuine-alloc-raw-reservers '(nl_wasm_arena_init nl_wasm_intern_region_init))
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
                             (equal (plist-get record :raw-reservers) '(nl_wasm_arena_init nl_wasm_intern_region_init)))
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



(provide 'wasm-intern-symbol)
