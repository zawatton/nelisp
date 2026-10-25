;;; nelisp-applyfn-name-split-test.el --- Resolver boundary -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'nelisp-standalone-build)

(defconst nelisp-name-split-test--read-only-ir
  '(imm ref arith cmp logic if shift ptr-read-u8 ptr-read-u64 let-rt))

(defun nelisp-name-split-test--check-ir (definition)
  "Reject every operation outside the literal matcher's no-call grammar."
  (let ((counts (make-hash-table :test #'eq)))
    (nelisp-aot-compiler--walk-ir
     (nelisp-aot-compiler--ir-get definition :body)
     (lambda (node)
       (let ((kind (nelisp-aot-compiler--ir-kind node)))
         (unless (memq kind nelisp-name-split-test--read-only-ir)
           (error "Resolver effect outside qualified grammar: %S" kind))
         (puthash kind (1+ (gethash kind counts 0)) counts))))
    counts))

(defun nelisp-name-split-test--parse (body)
  (let ((nelisp-aot-compiler--allow-external-user-calls t))
    (car (nelisp-aot-compiler--collect-defuns
          (nelisp-aot-compiler--parse
           `(defun nl_apply_builtin_name_key (name_ptr nl_name_len) ,body))))))

(ert-deftest nelisp-name-split-structural-admission-and-flat-control ()
  (let ((process-environment (copy-sequence process-environment)))
    (setenv "NELISP_T104_FLAT_DISPATCH" nil)
    (should-error (nelisp-standalone--applyfn-split-name-resolver '(if 1 2 3)))
    (setenv "NELISP_T104_FLAT_DISPATCH" "1")
    (should (equal (nelisp-standalone--applyfn-split-name-resolver '(if 1 2 3))
                   '(nil if 1 2 3)))))

(ert-deftest nelisp-name-split-preserves-first-match-sid-and-fallback ()
  (let* ((process-environment (copy-sequence process-environment))
         (table (list (cons '(:lit "alpha") '(:word . 11))
                      (cons '(:u8 "alpha") '(:word . 22))
                      (cons '(:lit "car") '(:word . 33))))
         (calls 0))
    (setenv "NELISP_T104_FLAT_DISPATCH" nil)
    (cl-letf (((symbol-function 'nelisp-standalone--sid-read-form)
               (lambda (_) 'test_sid))
              ((symbol-function 'sexp-tag) (lambda (_) 4))
              ((symbol-function 'nl_word_length) #'length)
              ((symbol-function 'sexp-name-eq)
               (lambda (a b) (if (equal a b) 1 0))))
      (let* ((old (nelisp-standalone--applyfn-build-dispatch table 1 t))
             (parts (nelisp-standalone--applyfn-split-name-resolver old))
             (helper (nth 3 (car parts))))
        (cl-letf (((symbol-function 'nl_apply_builtin_name_key)
                   (lambda (name len)
                     (setq calls (1+ calls))
                     (eval helper `((name_ptr . ,name) (nl_name_len . ,len))))))
          (dolist (case '(("alpha" 0 11) ("car" 0 33) ("unknown" 0 0) ("" 0 0)))
            (let ((bindings `((name_ptr . ,(car case)) (test_sid . ,(cadr case)))))
              (should (= (eval old bindings) (nth 2 case)))
              (should (= (eval (cdr parts) bindings) (nth 2 case)))))
          (setq calls 0)
          (let ((bindings `((name_ptr . "car")
                            (test_sid . ,(nelisp-standalone--sid "car")))))
            (should (= (eval (cdr parts) bindings) 33))
            (should (= calls 0))))))))

(ert-deftest nelisp-name-split-assembly-preserves-n5-order-and-one-boundary ()
  (let* ((process-environment (copy-sequence process-environment))
         (nelisp-standalone--target 'linux-x86_64)
         (table (list (cons '(:lit "alpha") '(:word . 11)))))
    (setenv "NELISP_T104_FLAT_DISPATCH" nil)
    (let ((source (nelisp-standalone--applyfn-assemble nil table))
          seen)
      (should (= 1 (cl-count-if
                    (lambda (f) (eq (cadr f) 'nl_apply_builtin_name_key)) (cdr source))))
      (should (memq 'nl_apply_builtin_name_key nelisp-standalone--inline-boundaries))
      (cl-letf (((symbol-function 'nelisp-standalone--n5-syscall-dispatch)
                 (lambda (d) (setq seen d) d)))
        (let* ((wrapped (nelisp-standalone--applyfn-assemble nil table))
               (outer (assq 'nelisp_apply_function_w
                            (mapcar #'cdr (cdr wrapped)))))
          (should outer)))
      ;; Alpha has no frozen SID: its separate private group declines. The
      ;; single N5 rewrite receives the unchanged list body/default.
      (should-not (nelisp-standalone--sid "alpha"))
      (should (equal seen
                     (nelisp-standalone--applyfn-build-dispatch
                      '(((:lit "alpha") :word . 11))
                      '(:word nl_applyfn_default_w func_ptr args env name_ptr)
                      t))))
    (setenv "NELISP_T104_FLAT_DISPATCH" "1")
    (should-not (cl-find-if
                 (lambda (f) (eq (cadr f) 'nl_apply_builtin_name_key))
                 (cdr (nelisp-standalone--applyfn-assemble nil table 1))))))

(ert-deftest nelisp-name-split-effect-check-rejects-alloc-park-and-generic-read ()
  (dolist (bad '((alloc-bytes 8 8)
                 (nl_thread_maybe_park 0)
                 (nl_string_mapping_op name_ptr 0 3)
                 (heap-read-u64 name_ptr 24)
                 (extern-call nl_val_tag name_ptr)
                 (ptr-write-u64 name_ptr 0 1)))
    (should-error
     (nelisp-name-split-test--check-ir
      (nelisp-name-split-test--parse (nelisp-heap-accessors-lower bad)))))
  (should (hash-table-p
           (nelisp-name-split-test--check-ir
            (nelisp-name-split-test--parse
             (nelisp-heap-accessors--lower-string-name-eq 'name_ptr "alpha"))))))

(ert-deftest nelisp-name-split-direct-only-is-cache-bound ()
  (let* ((source '(seq (defun small (a) (+ a 1))))
         (nelisp-aot-compiler--x86-direct-only-defuns nil)
         (plain (nelisp-standalone--unit-cache-key source)))
    (let ((nelisp-aot-compiler--x86-direct-only-defuns '(nl_apply_builtin_name_key)))
      (should-not (equal plain (nelisp-standalone--unit-cache-key source)))))
  (should (memq 'nl_apply_builtin_name_key nelisp-standalone--direct-only-reader-defuns))
  (should (memq 'nl_apply_builtin_name_key (nelisp-standalone--direct-only-for "applyfn.o"))))

(ert-deftest nelisp-name-split-xinline-keeps-existing-policy ()
  (let* ((nelisp-standalone--xinline-table (make-hash-table :test #'eq))
         (nelisp-standalone--xinline-used (make-hash-table :test #'eq))
         (source '(seq (defun small (a) (nl_string_read64 a 0)))))
    (puthash 'nl_string_read64 '((p off) . (+ p off)) nelisp-standalone--xinline-table)
    (should-not (equal source (nelisp-standalone--xinline-expand source)))
    ;; Global direct-control boundaries must not become a new xinline policy.
    (puthash 'nl_apply_function_w '((a) . (+ a 1)) nelisp-standalone--xinline-table)
    (let ((unselected '(seq (defun small (a) (nl_apply_function_w a)))))
      (should-not (equal unselected (nelisp-standalone--xinline-expand unselected))))))

(ert-deftest nelisp-name-split-unselected-unit-bytes-identical ()
  (let* ((nelisp-standalone--target 'linux-x86_64)
         (source '(seq (defun nl_string_read64 (p off) (+ p off))
                       (defun small (a) (nl_string_read64 a 0))))
         (normal (nelisp-standalone--compile-to-unit "small.o" source)))
    (let ((nelisp-standalone--inline-boundaries
           (cl-set-difference nelisp-standalone--inline-boundaries
                              '(nl_apply_builtin_name_key nl_apply_builtin_group_0_w nl_apply_builtin_group_1_w))))
      (should (equal normal (nelisp-standalone--compile-to-unit "small.o" source))))))

(ert-deftest nelisp-handler-group-route-and-default-exactly-once ()
  (let* ((process-environment (copy-sequence process-environment))
         (tree (nelisp-standalone--key-tree 'nl_dk '((1 . 11) (2 . 22) (3 . 33) (4 . 44)) 99))
         (parts (nelisp-standalone--applyfn-split-handler-groups `(let* () ,tree)))
         (defs (car parts)) (calls 0) (observed-mode nil))
    (cl-letf (((symbol-function 'nl_apply_builtin_group_0_w)
               (lambda (key f a e n p) (setq calls (1+ calls) observed-mode p)
                 (eval (nth 3 (car defs)) `((nl_dk . ,key) (func_ptr . ,f) (args . ,a) (env . ,e) (name_ptr . ,n) (_pad . ,p)))))
              ((symbol-function 'nl_apply_builtin_group_1_w)
               (lambda (key f a e n p) (setq calls (1+ calls) observed-mode p)
                 (eval (nth 3 (cadr defs)) `((nl_dk . ,key) (func_ptr . ,f) (args . ,a) (env . ,e) (name_ptr . ,n) (_pad . ,p))))))
      (dolist (mode '(0 1 3 256))
        (dolist (key '(0 1 2 3 4 5 9999999))
          (setq calls 0 observed-mode nil)
          (let ((bindings `((nl_dk . ,key) (func_ptr . 1) (args . 2) (env . 3)
                            (name_ptr . 4) (_pad . ,mode))))
            (should (= (eval tree bindings) (eval (cdr parts) bindings)))
            (should (= calls 1))
            (should (= observed-mode mode))))))))

(ert-deftest nelisp-handler-group-rejects-missing-input-and-inconsistent-default ()
  (should-error (nelisp-standalone--applyfn-handler-cost '(+ missing-input 1)))
  (should-error (nelisp-standalone--applyfn-split-handler-groups
                 '(let* () (if (< nl_dk 2) (if (= nl_dk 1) 11 90) (if (= nl_dk 2) 22 91)))))
  (let ((nelisp-standalone--target 'linux-x86_64))
    (dolist (name '(nelisp_apply_function_w nl_apply_builtin_group_0_w nl_apply_builtin_group_1_w))
      (should (memq name (nelisp-standalone--zero-fresh-frame-functions)))
      (when (not (eq name 'nelisp_apply_function_w))
        (should (memq name nelisp-standalone--inline-boundaries))))))

(defun nelisp-name-split-test--leaf (tree key)
  (pcase tree
    (`(if (< nl_dk ,pivot) ,left ,right)
     (nelisp-name-split-test--leaf (if (< key pivot) left right) key))
    (`(if (= nl_dk ,value) ,body ,default) (if (= key value) body default))
    (_ (error "Unexpected oracle key tree"))))

(ert-deftest nelisp-handler-group-every-production-key-keeps-original-arm ()
  (let* ((process-environment (copy-sequence process-environment))
         table)
    (setenv "NELISP_READER_DYNAMIC" "1")
    (setenv "NELISP_T104_FLAT_DISPATCH" nil)
    (cl-letf (((symbol-function 'nelisp-standalone--applyfn-assemble)
               (lambda (_helpers entries &optional _default) (setq table entries) '(seq))))
      (nelisp-standalone--applyfn-source))
    (let* ((original (nelisp-standalone--n5-syscall-dispatch
                      (nelisp-standalone--applyfn-build-dispatch table nil t)))
           (name-parts (nelisp-standalone--applyfn-split-name-resolver original))
           (parts (nelisp-standalone--applyfn-split-handler-groups (cdr name-parts)))
           (route (cadr (cadr (nth 2 (cdr parts)))))
           (old (nth 2 original))
           (keys (append '(-1 0 9999999)
                         (mapcar (lambda (n) (plist-get n :key))
                                 (append (nelisp-name-split-test--names) nil)))))
      (dolist (key keys)
        (let* ((group (eval route `((nl_dk . ,key))))
               (body (nth 3 (nth group (car parts)))))
          (should (memq group '(0 1)))
          (should (equal (nelisp-name-split-test--leaf old key)
                         (nelisp-name-split-test--leaf body key))))))))

(defun nelisp-name-split-test--sha (file)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (secure-hash 'sha256 (current-buffer))))

(defun nelisp-name-split-test--names ()
  (let ((seen (make-hash-table :test #'equal)) (index 0) result table)
    (cl-letf (((symbol-function 'nelisp-standalone--applyfn-assemble)
               (lambda (_helpers entries &optional _default) (setq table entries) '(seq))))
      (nelisp-standalone--applyfn-source))
    (dolist (entry table)
      (let ((name (cadr (car entry))))
        (unless (gethash name seen)
          (puthash name t seen)
          (push (list :name name :key (or (nelisp-standalone--sid name) (+ 1000000 index))) result)))
      (setq index (1+ index)))
    (vconcat (nreverse result))))

(defun nelisp-name-split-test--qualify-owner-context ()
  "Compile actual reader owner contexts without building a standalone binary."
  (let* ((process-environment (copy-sequence process-environment))
         (nelisp-standalone--target 'linux-x86_64)
         (max-lisp-eval-depth 40000) (max-specpdl-size 80000)
         (nelisp-standalone--inline-import-sources
          (nelisp-standalone--unit-inline-imports "applyfn-reader.o"))
         (nelisp-aot-compiler--x86-direct-only-defuns
          nelisp-standalone--direct-only-reader-defuns)
         (nelisp-aot-compiler--allow-external-user-calls t)
         (emit (symbol-function 'nelisp-aot-compiler--emit-defun))
         (input-files '("scripts/nelisp-standalone-build.el" "lisp/nelisp-aot-compiler.el"
                        "scripts/nelisp-boot-image.el" "lisp/nelisp-heap-accessors.el"
                        "lisp/nelisp-cc-string-blocks.el" "lisp/nelisp-cc-nlstr-direct-ops.el"))
         (input-before (mapcar (lambda (p) (cons p (nelisp-name-split-test--sha p))) input-files))
         (test-before (mapcar (lambda (p) (cons p (nelisp-name-split-test--sha p)))
                             '("test/nelisp-applyfn-name-split-test.el"
                               "test/nelisp-applyfn-name-split-native.py")))
         (cache "target/dispatch-name-split/owner-ir.el")
         (reference "target/dispatch-name-split/tree-only-history/owner-ir.el")
         (frames nil) resolver outer groups helper-source baseline-source)
    (dolist (v '("NELISP_INLINE" "NELISP_XINLINE" "NELISP_INLINE_MAX_NODES"
                 "NELISP_T104_FLAT_DISPATCH" "NELISP_TCO"))
      (when (getenv v) (error "Nondefault qualification configuration: %s" v)))
    (setenv "NELISP_READER_DYNAMIC" "1")
    (if (file-exists-p cache)
        (let ((data (with-temp-buffer (insert-file-contents cache) (read (current-buffer)))))
          (unless (equal input-before (plist-get data :inputs)) (error "Stale owner IR cache"))
          (setq frames (plist-get data :frames) resolver (plist-get data :resolver)
                outer (plist-get data :outer) groups (plist-get data :groups)))
      (let ((source (nelisp-standalone--patch-autoload-lookups
                   (nelisp-standalone--applyfn-source))))
      ;; Retain the measured monolithic/TREE-only history; lower only the
      ;; refined owner, never rerun an unchanged expensive original phase.
      (let* ((prior (with-temp-buffer (insert-file-contents reference) (read (current-buffer))))
             (prior-frames (plist-get prior :frames)))
        (setq frames (list (cl-find-if (lambda (f) (equal (plist-get f :phase) "monolithic")) prior-frames))))
      (setq helper-source (cl-find-if
                           (lambda (f) (eq (cadr f) 'nl_apply_builtin_name_key)) (cdr source)))
      (cl-letf (((symbol-function 'nelisp-standalone--applyfn-split-name-resolver)
                 (lambda (d) (cons nil d))))
        (setq baseline-source
              (nelisp-standalone--patch-autoload-lookups
               (nelisp-standalone--applyfn-source))))
      (dolist (entry `(("split" . ,source)))
        (let ((phase (car entry)))
          (cl-letf (((symbol-function 'nelisp-aot-compiler--emit-defun)
                     (lambda (ir _buf)
                       (let ((name (nelisp-aot-compiler--ir-get ir :name)))
                         (when (memq name '(nelisp_apply_function_w nl_apply_builtin_name_key
                                           nl_apply_builtin_group_0_w nl_apply_builtin_group_1_w))
                           (push (list :phase phase :name (symbol-name name)
                                       :rt_slots (nelisp-aot-compiler--ir-get ir :rt-slot-count)) frames)
                           (when (equal phase "split")
                             (cond ((eq name 'nl_apply_builtin_name_key) (setq resolver ir))
                                   ((eq name 'nelisp_apply_function_w) (setq outer ir))
                                   (t (push ir groups)))))))))
            (nelisp-standalone--compile-to-unit "applyfn-reader.o" (cdr entry))))))
      (make-directory "target/dispatch-name-split" t)
      (let ((print-length nil) (print-level nil) (print-circle t))
        (with-temp-file cache
          (prin1 (list :inputs input-before :frames frames :resolver resolver :outer outer :groups groups)
                 (current-buffer)))))
    (princ (format "OWNER-FRAMES %S\n" (reverse frames)))
    (unless (and resolver outer (= (length groups) 2)) (error "Production optimizer erased a dispatch boundary"))
    (let ((calls 0) (group-calls 0))
      (nelisp-aot-compiler--walk-ir
       (nelisp-aot-compiler--ir-get outer :body)
       (lambda (ir)
         (when (and (eq (nelisp-aot-compiler--ir-kind ir) 'call)
                    (memq (nelisp-aot-compiler--ir-get ir :name)
                          '(nl_apply_builtin_group_0_w nl_apply_builtin_group_1_w)))
           (setq group-calls (1+ group-calls)))
         (when (and (eq (nelisp-aot-compiler--ir-kind ir) 'call)
                    (eq (nelisp-aot-compiler--ir-get ir :name) 'nl_apply_builtin_name_key))
           (setq calls (1+ calls)))))
      (unless (= calls 1) (error "Expected one retained resolver call, got %s" calls))
      (unless (= group-calls 2) (error "Expected both out-of-line handler calls")))
    (let* ((counts (nelisp-name-split-test--check-ir resolver))
           (buf (nelisp-asm-x86_64-make-buffer 'sysv))
           (nelisp-aot-compiler--abi 'sysv)
           (nelisp-aot-compiler--arch 'x86_64)
           (nelisp-aot-compiler-zero-fresh-frame-functions
            (nelisp-standalone--zero-fresh-frame-functions))
           pairs group-records)
      (unless (equal nelisp-aot-compiler-zero-fresh-frame-functions
                     '(nelisp_apply_function_w nl_apply_builtin_group_0_w nl_apply_builtin_group_1_w
                       nl_apply_builtin_frame_group_0_w nl_apply_builtin_frame_group_1_w
                       nl_apply_builtin_frame_w nl_eic_argframe_call_w
                       bf_garbage_collect_w nl_gc_stats_run))
        (error "Initialized outer selection changed"))
      (make-directory "target/dispatch-name-split" t)
      (let* ((outer-buf (nelisp-asm-x86_64-make-buffer 'sysv))
             (rt (nelisp-aot-compiler--ir-get outer :rt-slot-count))
             (zero (nelisp-asm-x86_64-make-buffer 'sysv)))
        ;; Keep the measured production frame and four-argument ABI while
        ;; emitting a bounded entry control. Full handler emission belongs
        ;; to the coordinator's actual standalone build.
        (funcall emit (nelisp-aot-compiler--ir-with
                       outer :body (nelisp-aot-compiler--make-ir 'imm :value 0 :class 'gp))
                 outer-buf)
        (nelisp-aot-compiler--emit-zero-fresh-rt-slots zero (+ rt (logand rt 1)))
        (let ((code (nelisp-asm-x86_64-resolve-fixups outer-buf)))
          (unless (string-match-p
                   (regexp-quote (nelisp-asm-x86_64-resolve-fixups zero)) code)
            (error "Actual outer emission lost its exact frame initializer"))
          (let ((coding-system-for-write 'binary))
            (with-temp-file "target/dispatch-name-split/dispatcher-prologue.bin"
              (set-buffer-multibyte nil) (insert code)))))
      (funcall emit resolver buf)
      (unless (null (nelisp-asm-x86_64-extract-relocs buf))
        (error "Resolver has emitted relocations"))
      (let ((coding-system-for-write 'binary))
        (with-temp-file "target/dispatch-name-split/resolver.bin"
          (set-buffer-multibyte nil) (insert (nelisp-asm-x86_64-resolve-fixups buf))))
      (maphash (lambda (k v) (push (cons k v) pairs)) counts)
      (dolist (group groups)
        (let* ((name (nelisp-aot-compiler--ir-get group :name))
               (slots (nelisp-aot-compiler--ir-get group :rt-slot-count))
               (params '(nl_dk func_ptr args env name_ptr _pad))
               (body '(if (= nl_dk 0)
                          (ptr-read-u64 (+ (aot-current-sp) 16) 0)
                        (if (= nl_dk 1)
                            (seq (ptr-write-u64 (+ (aot-current-sp) 16) 0 3735928559)
                                 (+ func_ptr (+ args (+ env (+ name_ptr _pad)))))
                          (+ nl_dk (+ func_ptr (+ args (+ env (+ name_ptr _pad))))))))
               (probe (car (nelisp-aot-compiler--collect-defuns
                            (nelisp-aot-compiler--parse `(defun ,name ,params ,body)))))
               (probe (nelisp-aot-compiler--ir-with probe :rt-slot-count slots))
               (loop-buf (nelisp-asm-x86_64-make-buffer 'sysv))
               hashes)
          (unless (= (length (nelisp-aot-compiler--ir-get group :params)) 6)
            (error "Unexpected handler group ABI"))
          (dolist (control '(nil t))
            (let* ((entry (nelisp-asm-x86_64-make-buffer 'sysv))
                   (file (format "target/dispatch-name-split/%s-%s.bin" name (if control "control" "initialized")))
                   (coding-system-for-write 'binary))
              (if control
                  (cl-letf (((symbol-function 'nelisp-aot-compiler--emit-zero-fresh-rt-slots) (lambda (&rest _) nil)))
                    (funcall emit probe entry))
                (funcall emit probe entry))
              (with-temp-file file
                (set-buffer-multibyte nil) (insert (nelisp-asm-x86_64-resolve-fixups entry)))
              (push (cons file (nelisp-name-split-test--sha file)) hashes)))
          (nelisp-aot-compiler--emit-zero-fresh-rt-slots loop-buf (+ slots (logand slots 1)))
          (let ((file (format "target/dispatch-name-split/%s-loop.bin" name))
                (coding-system-for-write 'binary))
            (with-temp-file file
              (set-buffer-multibyte nil) (insert (nelisp-asm-x86_64-resolve-fixups loop-buf)))
            (push (cons file (nelisp-name-split-test--sha file)) hashes))
          (push (list :name (symbol-name name) :rt_slots slots :params 6 :files hashes) group-records)))
      (unless (equal input-before
                     (mapcar (lambda (p) (cons p (nelisp-name-split-test--sha p))) input-files))
        (error "Production input changed during qualification"))
      (unless (equal test-before
                     (mapcar (lambda (p) (cons (car p) (nelisp-name-split-test--sha (car p)))) test-before))
        (error "Qualification test changed during execution"))
      (let ((coding-system-for-write 'utf-8))
        (with-temp-file "target/dispatch-name-split/qualification.json"
          (insert (json-encode
                   (list :status "MECHANICAL_ONLY" :frames (vconcat (reverse frames))
                         :resolver_ir_counts pairs :resolver_relocations 0 :outer_calls 1
                         :outer_initialized t
                         :groups (vconcat group-records) :outer_group_calls 2
                         :resolver_sha256 (nelisp-name-split-test--sha "target/dispatch-name-split/resolver.bin")
                         :dispatcher_scope "actual owner frame and production prologue; immediate-body control"
                         :dispatcher_sha256 (nelisp-name-split-test--sha "target/dispatch-name-split/dispatcher-prologue.bin")
                         :source_sha256 input-before :source_after_sha256 input-before
                         :test_sha256 test-before
                         :base_head (car (process-lines "git" "rev-parse" "HEAD"))
                         :reference_ir_sha256 (nelisp-name-split-test--sha reference)
                         :modified_tracked_el
                         (vconcat (process-lines "git" "diff" "--name-only" "--" "*.el"))
                         :names (nelisp-name-split-test--names))))))
      (princ (format "OWNER-FRAMES %S\n" (reverse frames))))))

(provide 'nelisp-applyfn-name-split-test)
