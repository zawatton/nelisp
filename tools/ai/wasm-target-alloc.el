;;; wasm-target-alloc.el --- Genuine one-chunk allocator composition -*- lexical-binding: t; -*-
;; trusted compiler inputs are the running Emacs installation lisp directory and the eight named repository compiler files, hash-checked. Harness sources under tools/ai are trusted by review (the lane diff gate); attacks that require the harness itself to write files or tamper with process state at run time are out of scope. The checker defends against drift in genuine sources and artifacts, manifest inconsistency, and accidental environment contamination (session definitions, load-path shadowing).
(require 'wasm-target-init)
(defconst wasm-target-alloc-inputs
  '(:target wasm32 :allocator-source linux-x86_64 :runtime-reload nil :callsite nil
    :arena-size 1048576 :intern-region excluded :growth none))
(defconst wasm-target-alloc-adapters
  '((defun nl_os_alloc_chunk (_size) 0)
    (defun nl_os_alloc_fail ()
      (ptr-write-u64 (data-addr wasm_target_alloc_failure) 0 88))))
(defconst wasm-target-alloc--primitives
  '(+ - * / % = < > <= >= logand logior logxor shl sar
    ptr-read-u64 ptr-write-u64 atomic-compare-exchange))
(defvar wasm-target-alloc--oracle-cache nil)
(defun wasm-target-alloc-oracle-record ()
  "Read genuine arena and membership providers in an isolated child."
  (or (and wasm-target-alloc--oracle-cache
           (wasm-rootstack--oracle-files-current-p
            (plist-get wasm-target-alloc--oracle-cache :files)
            (list (expand-file-name "tools/ai/wasm-target-alloc-oracle.el" wasm-rootstack--root)))
           wasm-target-alloc--oracle-cache)
      (let* ((root wasm-rootstack--root)
             (helper (expand-file-name "tools/ai/wasm-target-alloc-oracle.el" root))
             (build (expand-file-name "scripts/nelisp-standalone-build.el" root))
             (process-environment (copy-sequence process-environment)))
        (setenv "NELISP_RUNTIME_RELOAD" nil)
        (with-temp-buffer
          (let ((rc (apply #'call-process "bash" nil t nil
                           (append (list "-c" "ulimit -v 8388608; exec \"$@\"" "oracle" "timeout" "120"
                                         (expand-file-name invocation-name invocation-directory) "-Q" "--batch")
                                   (cl-loop for dir in '("lisp" "src" "scripts") append (list "-L" (expand-file-name dir root)))
                                   (list "--eval" "(setq load-prefer-newer t)" "-l" helper "--eval"
                                         (prin1-to-string `(wasm-target-alloc-oracle ,build)))))))
            (unless (equal rc 0) (error "Allocator oracle rc=%s %s" rc (buffer-string))))
          (goto-char (point-min))
          (unless (re-search-forward "^WASM-TARGET-ALLOC=" nil t) (error "Allocator oracle missing record"))
          (let* ((record (read (current-buffer))) (files (plist-get record :files)))
            (unless (and (eq (plist-get record :allocator-source) 'linux-x86_64)
                         (assoc build files) (assoc helper files)
                         (wasm-rootstack--oracle-files-current-p files (list helper)))
              (error "Allocator oracle identity mismatch"))
            (setq wasm-target-alloc--oracle-cache record))))))

(defun wasm-target-alloc-calls (forms)
  "Walk executable FORMS, treating binding lists and clause lists as syntax."
  (let (calls)
    (cl-labels ((walk (form)
                  (when (consp form)
                    (unless (proper-list-p form) (error "Unsupported allocator syntax %S" form))
                    (pcase (car form)
                      ('quote nil)
                      ((or 'let 'let*)
                       (dolist (binding (cadr form))
                         (when (consp binding) (mapc #'walk (cdr binding))))
                       (mapc #'walk (cddr form)))
                      ('setq
                       (let ((pairs (cdr form)))
                         (while pairs
                           (unless (and (symbolp (car pairs)) (cdr pairs)) (error "Invalid allocator setq"))
                           (walk (cadr pairs)) (setq pairs (cddr pairs)))))
                      ('cond (dolist (clause (cdr form)) (mapc #'walk clause)))
                      ((or 'seq 'progn 'if 'while 'and 'or) (mapc #'walk (cdr form)))
                      ('data-addr
                       (unless (and (= (length form) 2) (symbolp (cadr form)))
                         (error "Unsupported allocator owner %S" form)))
                      (_ (unless (symbolp (car form)) (error "Unsupported allocator call %S" form))
                         (cl-pushnew (car form) calls) (mapc #'walk (cdr form)))))))
      (mapc #'walk forms))
    (sort calls (lambda (a b) (string< (symbol-name a) (symbol-name b))))))

(defun wasm-target-alloc-validate-adapters (adapters)
  "Require exactly the two OS boundaries before walking any provider."
  (let ((permitted '(nl_os_alloc_chunk nl_os_alloc_fail)) seen)
    (dolist (definition adapters)
      (let ((name (cadr definition)))
        (unless (and (eq (car definition) 'defun) (memq name permitted)
                     (not (memq name seen)))
          (error "Unpermitted allocator adapter %s" name))
        (push name seen)))
    (dolist (name permitted)
      (unless (memq name seen) (error "Unpermitted allocator adapter %s" name)))))

(cl-defun wasm-target-alloc-closure (record &optional (adapters nil adapted-p))
  "Select a checked closure by name; ADAPTERS replace only explicit OS boundaries."
  (when adapted-p (wasm-target-alloc-validate-adapters adapters))
  (let* ((pool (append (cdr (plist-get record :arena)) (cdr (plist-get record :support))))
         (pending '(nl_alloc_bytes)) seen definitions)
    (while pending
      (let ((name (pop pending)))
        (unless (memq name seen)
          (push name seen)
          (let ((definition (or (assq name (mapcar (lambda (d) (cons (cadr d) d)) adapters))
                                (unless (and adapted-p (memq name '(nl_os_alloc_chunk nl_os_alloc_fail)))
                                  (assq name (mapcar (lambda (d) (cons (cadr d) d)) pool))))))
            (unless definition (error "Unresolved allocator callee %s" name))
            (setq definition (cdr definition))
            (push definition definitions)
            (dolist (callee (wasm-target-alloc-calls (nthcdr 3 definition)))
              (cond ((memq callee wasm-target-alloc--primitives) nil)
                    ((eq callee 'syscall-direct)
                     (unless (memq name '(nl_os_alloc_chunk nl_os_alloc_fail))
                       (error "Non-OS allocator definition %s needs syscall-direct" name)))
                    (t (push callee pending))))))))
    (sort definitions (lambda (a b) (string< (symbol-name (cadr a)) (symbol-name (cadr b)))))))

(defun wasm-target-alloc-authority ()
  "Compose init owners and every owner used by the complete allocator closure."
  (let ((wasm-rootstack--boundaries
         (append wasm-rootstack--boundaries wasm-target-init--boundaries
                 '((nl_alloc_check . nl_bt_snapshot) (nl_alloc_diag . nl_gc_reclaim_scratch)
                   (nl_gc_stats . nl_aref_cache_table) (nl_gc_diag . nl_gc_loop_ctx)
                   (nl_freelist_large_bins . nl_gc_start_index))))
        (wasm-rootstack--oracle-cache (make-hash-table :test #'equal)))
    (wasm-rootstack-authority :layout-inputs '(:target wasm32 :runtime-reload nil :callsite nil))))

(defun wasm-target-alloc-source (contract owners definitions &optional skip)
  "Compose target init unchanged, the genuine DEFINITIONS and failure word."
  (let* ((init (cdr (wasm-target-init--source contract owners skip)))
         (init (cl-remove-if (lambda (form) (memq (cadr form) '(nl_freelist_small_mask_ptr wasm_target_init_end address-end))) init)))
    `(seq ,@init
          (data-blob wasm_target_alloc_failure ,(make-string 8 0) bss)
          (defun address-wasm_target_alloc_failure () (data-addr wasm_target_alloc_failure))
          ;; This sentinel follows the failure owner too.
          (data-blob wasm_target_alloc_end ,(make-string 8 0) bss)
          (defun address-end () (data-addr wasm_target_alloc_end))
          ,@definitions)))

;; Closed schema authorities are deliberately separate from both composers.
(defconst wasm-target-alloc--closure-names
  '(nl_align_up nl_alloc_bytes nl_alloc_bytes_uncheck
    nl_alloc_check_enabled nl_alloc_check_size nl_alloc_check_stamp
    nl_alloc_diag_bucket nl_alloc_diag_bump nl_alloc_diag_linear
    nl_alloc_zero_fill nl_block_total nl_chunk_alloc_new
    nl_chunk_cursor_addr nl_chunk_init_descriptor nl_chunk_size_for
    nl_chunk_try_alloc nl_fl_record_trip nl_freelist_bucket_pop
    nl_freelist_bucket_split nl_freelist_large_bin nl_freelist_large_head
    nl_freelist_scan nl_freelist_scan_drop_tail nl_freelist_scan_head
    nl_freelist_scan_large nl_freelist_scan_large_from nl_freelist_small_mask_bit
    nl_freelist_small_mask_clear nl_freelist_small_mask_ptr nl_freelist_small_mask_set
    nl_freelist_small_mask_sync_head nl_freelist_small_next_index nl_freelist_split_tail
    nl_freelist_take nl_gc_in_arena nl_hdr_bt
    nl_hdr_mark nl_hdr_set_mark nl_os_alloc_chunk
    nl_os_alloc_fail nl_seq2))
(defconst wasm-target-alloc--owner-names
  '(nl_rootstack_top nl_rootstack_region nl_thread_parallel_ctx
    nl_thread_registry nl_root_pin_control nl_root_pin_region nl_arena_base
    nl_freelist_small_mask nl_alloc_check nl_alloc_diag nl_gc_stats nl_gc_diag
    nl_freelist_large_bins))

(defun wasm-target-alloc-validate-compiler-input (path contract owners genuine adapters)
  "Read and validate the exact saved PATH against a closed independent schema.
GENUINE is the recorded oracle closure before adaptation/rewrite.  Do not call
`wasm-target-alloc-source' or `wasm-target-init--source' here: they are the
composers under test.  Return the validated compiler input."
  (let ((source
         (condition-case nil
             (with-temp-buffer
               (insert-file-contents path)
               (let ((form (read (current-buffer))))
                 (skip-chars-forward " \t\r\n")
                 (unless (eobp) (error "Trailing compiler input"))
                 form))
           (error (error "Unexpected compiler input form: unreadable or trailing input"))))
        expected seen)
    (unless (and (proper-list-p source) (eq (car-safe source) 'seq))
      (error "Unexpected compiler input form: expected seq"))
    (unless (equal (mapcar (lambda (row) (plist-get row :name)) owners)
                   wasm-target-alloc--owner-names)
      (error "Unexpected compiler input form: owner authority membership"))
    (unless (equal (sort (mapcar #'cadr genuine)
                        (lambda (a b) (string< (symbol-name a) (symbol-name b))))
                   wasm-target-alloc--closure-names)
      (error "Unexpected compiler input form: genuine closure membership"))
    (wasm-target-alloc-validate-adapters adapters)
    ;; Required storage, including the failure word and final sentinel, once.
    (dolist (owner (append owners '((:name wasm_target_alloc_failure :size 8)
                                    (:name wasm_target_alloc_end :size 8))))
      (let ((name (plist-get owner :name)))
        (push (cons name `(data-blob ,name ,(make-string (plist-get owner :size) 0) bss))
              expected)))
    ;; Exactly these 41 definitions. The two recorded OS rows substitute by
    ;; name; all other bodies come directly from the recorded genuine closure.
    ;; nl_freelist_small_mask_ptr is already one of these genuine definitions,
    ;; so the harness must not introduce another copy.
    (dolist (name wasm-target-alloc--closure-names)
      (let ((definition (cl-find name (if (memq name '(nl_os_alloc_chunk nl_os_alloc_fail))
                                         adapters genuine) :key #'cadr)))
        (push (cons name (car (wasm-target-init-rewrite (list definition)))) expected)))
    ;; Fixed harness: address-OWNER for the 13 owners and failure word,
    ;; address-end, arena-size, nl_wasm_arena_init, nl_rootstack_init. No others.
    (dolist (name (append wasm-target-alloc--owner-names '(wasm_target_alloc_failure)))
      (let ((export (intern (concat "address-" (symbol-name name)))))
        (push (cons export `(defun ,export () (data-addr ,name))) expected)))
    (dolist (definition
             (list '(defun address-end () (data-addr wasm_target_alloc_end))
                   `(defun arena-size () ,(plist-get (plist-get contract :inputs) :arena-size))
                   (wasm-target-init--init-definition contract)
                   (cl-find 'nl_rootstack_init (cdr nelisp-cc-rootstack--source) :key #'cadr)))
      (push (cons (cadr definition) definition) expected))
    (dolist (form (cdr source))
      (let* ((name (and (proper-list-p form) (cadr form)))
             (row (assq name expected)))
        (unless (and (memq (car-safe form) '(data-blob defun)) row
                     (not (memq name seen)) (equal form (cdr row)))
          (error "Unexpected compiler input form: %S %S" (car-safe form) name))
        (push name seen)))
    (dolist (row expected)
      (unless (memq (car row) seen)
        (error "Unexpected compiler input form: missing %S" (car row))))
    source))

(defconst wasm-target-alloc-compiler-repository-files
  '("lisp/nelisp-aot-compiler.el"
    "lisp/nelisp-asm-arm64.el"
    "lisp/nelisp-asm-wasm.el"
    "lisp/nelisp-asm-x86_64.el"
    "lisp/nelisp-elf-write.el"
    "lisp/nelisp-sexp-layout.el"
    "lisp/nelisp-standalone-arena-rewrite.el"
    "lisp/nelisp-wasm-write.el")
  "Exact repository files reached by the clean compiler's loads and requires.")

(defun wasm-target-alloc-emacs-lisp-root ()
  "Derive the running Emacs installation's Lisp root without load-path lookup."
  (file-name-as-directory
   (file-truename (if (boundp 'lisp-directory) lisp-directory
                    (expand-file-name "../lisp" data-directory)))))

(defun wasm-target-alloc-check-library-shadows ()
  "Reject repository Lisp library names that collide with the installation.
Check before launching the compiler, independently of its load-history."
  (let ((libraries (make-hash-table :test #'equal)))
    (dolist (file (directory-files-recursively
                   (wasm-target-alloc-emacs-lisp-root) "\\.elc?\\(?:\\.gz\\)?\\'"))
      (puthash (replace-regexp-in-string "\\.elc?\\(?:\\.gz\\)?\\'" ""
                                           (file-name-nondirectory file)) t libraries))
    (dolist (file (directory-files (expand-file-name "lisp" wasm-rootstack--root)
                                 nil "\\.elc?\\'"))
      (let ((name (file-name-sans-extension file)))
        (when (gethash name libraries)
          (error "Repository library shadows installation library %s" name))))))

(defun wasm-target-alloc-compiler-files (files installation-root &optional diagnostic)
  "Allow only the named repository compiler FILES and INSTALLATION-ROOT.
The recorded installation root must equal the running Emacs's own root.
DIAGNOSTIC permits an early compiler error before all lazy requires run."
  (let* ((root (wasm-target-alloc-emacs-lisp-root))
         (repository (file-name-as-directory (file-truename wasm-rootstack--root)))
         (allowed (mapcar (lambda (file) (expand-file-name file wasm-rootstack--root))
                          wasm-target-alloc-compiler-repository-files))
         (explicit (mapcar (lambda (file) (expand-file-name file wasm-rootstack--root))
                           '("lisp/nelisp-asm-wasm.el" "lisp/nelisp-aot-compiler.el")))
         (required (if diagnostic explicit allowed)))
    (unless (equal installation-root root)
      (error "Unexpected compiler child installation root %s" installation-root))
    (dolist (row files)
      (let* ((file (car row)) (canonical (file-truename file)))
        (when (or (string-match-p "\\.eln\\'" file)
                  (string-match-p "\\.eln\\'" canonical))
          (error "Native compiler child load forbidden %s" file))
        ;; Repository membership takes precedence, even for a forged broad root.
        (unless (if (string-prefix-p repository canonical)
                    (and (member file allowed) (equal file canonical))
                  (string-prefix-p root canonical))
          (error "Unexpected compiler child load %s" file))))
    (unless (and files
                 (= (length files) (length (delete-dups (mapcar (lambda (row)
                                                                (file-truename (car row))) files))))
                 (cl-every (lambda (file) (assoc file files)) required)
                 (wasm-rootstack--oracle-files-current-p files explicit))
      (error "Allocator compiler loaded-file identity or isolation mismatch"))
    files))

(defun wasm-target-alloc-compile-file (input output)
  "Compile saved INPUT in a clean child; return its hashed provenance.
The caller validates the schema first.  The parent enforces the named compiler
file allowlist and the runtime-derived Emacs installation root on recorded loads.
A prelaunch name check and appended load path prevent installation shadowing."
  (let* ((root wasm-rootstack--root)
         (process-environment (copy-sequence process-environment))
         ;; This expression is passed as data to -Q, never loaded as a helper.
         (expression
          `(let ((input ,(expand-file-name input)) (output ,(expand-file-name output))
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
               (prin1 (list :diagnostic diagnostic
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

(provide 'wasm-target-alloc)
