;;; wasm-rootstack-full-test.el --- Native pin layout checks -*- lexical-binding: t; -*-
(require 'ert)
(require 'wasm-rootstack-full-controls)

(ert-deftest wasm-rootstack/full-number-resolver ()
  (let* ((inputs wasm-rootstack--layout-inputs)
         (base (wasm-rootstack--number '(nelisp-standalone--driver-bss-base-size) inputs))
         (region (wasm-rootstack--number '(nelisp-standalone--root-pin-region-offset) inputs)))
    (should (= base (+ 57616 4194304 96 176 64 56 40 1040 64 192 64 40 8 64)))
    (should (= region (logand (+ base 64 31) -32)))
    (should (= (% region 32) 0))
    (should (>= (- region base) 56))
    (should (= (wasm-rootstack--number '(* nelisp-standalone--root-pin-slots 32) inputs) 524288))
    (should (= (wasm-rootstack--number '(logand (+ (* 3 8) 31) -32) inputs) 32))
    (dolist (form '(unknown (+ 1 unknown) (eval 1) (nelisp-standalone--driver-bss-base-size 1)
                   (if unknown 1 0)))
      (should (string-prefix-p "Unresolved ownership generator"
                               (error-message-string (should-error (wasm-rootstack--number form inputs))))))
    (should (string-prefix-p
             "Unresolved ownership generator: layout inputs nil"
             (error-message-string
              (should-error (wasm-rootstack--number '(nelisp-standalone--driver-bss-base-size))))))))

(ert-deftest wasm-rootstack/full-explicit-inputs-and-hashes ()
  (let* ((authority (wasm-rootstack-authority))
         (owners (plist-get authority :owners))
         (control (nth 4 owners)) (region (nth 5 owners)))
    (should (equal (plist-get authority :layout-inputs) wasm-rootstack--layout-inputs))
    (should (= (length (plist-get authority :predicate-sha256)) 2))
    (should (= (length (plist-get authority :generator-sha256)) 3))
    (dolist (entry (append (plist-get authority :predicate-sha256)
                           (plist-get authority :generator-sha256)))
      (should (string-match-p "\\`[[:xdigit:]]\\{64\\}\\'" (cdr entry))))
    (should (= (+ (plist-get control :offset) (plist-get control :size)) (plist-get region :offset)))
    (should (= (plist-get region :size) 524288))
    (should (= (+ (plist-get region :offset) (plist-get region :size)) (plist-get region :end)))
    (dolist (inputs '(nil (:target wasm32) (:target linux-x86_64 :runtime-reload nil :callsite nil)
                         (:target wasm32 :runtime-reload t :callsite nil)
                         (:target wasm32 :runtime-reload nil :callsite t)))
      (should (equal
               (format "Unresolved ownership generator: layout inputs %S" inputs)
               (error-message-string (should-error (wasm-rootstack-authority :layout-inputs inputs))))))))

(ert-deftest wasm-rootstack/full-predicate-syntax-gates ()
  (let ((reader (symbol-function 'wasm-rootstack--build-definition)))
    (dolist (change '(linux-x86_64 nelisp-standalone--runtime-reload-enabled-p))
      ;; Exercise the structural gate independently of the complete-definition pin.
      (cl-letf (((symbol-function 'wasm-rootstack--check-reviewed-definition)
                 (lambda (_name _definition) nil))
                ((symbol-function 'wasm-rootstack--build-definition)
                 (lambda (kind name)
                   (cl-subst 'changed change (funcall reader kind name)))))
        (should (string-prefix-p "Unresolved ownership generator"
                                 (error-message-string (should-error (wasm-rootstack-authority)))))))))

(ert-deftest wasm-rootstack/full-genuine-coverage ()
  (let* ((authority (wasm-rootstack-authority))
         (usage (wasm-rootstack--check-coverage (plist-get authority :owners) nelisp-cc-rootstack--source)))
    (should (= (cdr (assq 'nl_root_pin_control (plist-get usage :extents))) 56))
    (should (plist-get usage :pin-capacities))
    (dolist (capacity (plist-get usage :pin-capacities))
      (should (= (plist-get capacity :size) 524288)))
    (should (string-prefix-p
             "Genuine pin region capacity mismatch:"
             (error-message-string
              (should-error
               (wasm-rootstack--check-coverage
                (plist-get authority :owners)
                (cl-subst 524280 524288 nelisp-cc-rootstack--source))))))
    (should (equal
             "Genuine pin region capacity contract missing"
             (error-message-string
              (should-error
               (wasm-rootstack--check-coverage
                (plist-get authority :owners)
                (cl-subst 'unknown-capacity '+ nelisp-cc-rootstack--source))))))))

(ert-deftest wasm-rootstack/full-layout-and-absolute-alignment-report ()
  (let* ((owners (plist-get (wasm-rootstack-authority) :owners))
         (layout (wasm-rootstack-layout (wasm-rootstack-compose :owners owners :full t)))
         (alignment (plist-get layout :pin-region-alignment)))
    (should (= (plist-get alignment :required) 32))
    (should (= (plist-get alignment :remainder) (% (plist-get alignment :address) 32)))
    (should (equal (plist-get alignment :absolute-aligned) (= (plist-get alignment :remainder) 0)))
    (should (plist-get alignment :limit))
    (should (= (cl-count-if (lambda (form) (eq (car-safe form) 'defun))
                            (wasm-rootstack-compose :owners owners :full t)) 61))))

(ert-deftest wasm-rootstack/full-negative-controls ()
  (let ((ownership (should-error (wasm-rootstack-full-control 'ownership)
                                 :type 'nelisp-aot-compiler-error)))
    (should (equal ownership '(nelisp-aot-compiler-error :wasm-p3-external-data-addr "nl_root_pin_control"))))
  (dolist (entry '((undersize . "Rootstack usage coverage: nl_root_pin_control size=32 requires=56")
                   (alignment . "Native pin region alignment:")
                   (overlap . "Native rootstack ownership overlap:")
                   (unresolved-input . "Unresolved ownership generator: layout inputs nil")))
    (should (string-prefix-p (cdr entry)
                             (error-message-string (should-error (wasm-rootstack-full-control (car entry))))))))

(ert-deftest wasm-rootstack/r2-f3-complete-predicate-shape ()
  (let ((reader (symbol-function 'wasm-rootstack--build-definition)))
    (dolist (name '(nelisp-standalone--runtime-reload-enabled-p
                    nelisp-standalone--callsite-enabled-p))
      (dolist (change '(prologue epilogue name args duplicate-docstring))
        ;; Exercise the structural gate independently of the complete-definition pin.
	(cl-letf (((symbol-function 'wasm-rootstack--check-reviewed-definition)
                   (lambda (_name _definition) nil))
                  ((symbol-function 'wasm-rootstack--build-definition)
                   (lambda (kind requested)
                     (let ((definition (copy-tree (funcall reader kind requested))))
                       (if (not (eq requested name)) definition
                         (pcase change
                           ('prologue (append (cl-subseq definition 0 3)
                                              '((setq target 'linux-x86_64)) (nthcdr 3 definition)))
                           ('epilogue (append definition '((setq target 'linux-x86_64))))
                           ('name (setf (nth 1 definition) 'changed) definition)
                           ('args (setf (nth 2 definition) '(changed)) definition)
                           ('duplicate-docstring
                            (append (cl-subseq definition 0 3) '("extra docstring")
                                    (nthcdr 3 definition)))))))))
          (should (string-prefix-p
                   "Unresolved ownership generator: native layout predicate changed"
                   (error-message-string (should-error (wasm-rootstack-authority))))))))))

(ert-deftest wasm-rootstack/r2-f4-rebound-default-is-rejected ()
  (dolist (tuple '((:target windows-x86_64 :runtime-reload t :callsite nil)
                   (:target windows-x86_64 :runtime-reload nil :callsite nil)
                   (:target wasm32 :runtime-reload t :callsite nil)
                   (:target wasm32 :runtime-reload nil :callsite t)))
    (let ((wasm-rootstack--layout-inputs tuple))
      (should (string-prefix-p
               "Unresolved ownership generator: layout inputs"
               (error-message-string (should-error (wasm-rootstack-authority))))))))


(defun wasm-rootstack-full--reject-definitions (definitions message)
  "Append DEFINITIONS in memory and assert the specific real compose MESSAGE."
  (let* ((owners (plist-get (wasm-rootstack-authority) :owners))
         (nelisp-cc-rootstack--source
          (append (copy-tree nelisp-cc-rootstack--source) definitions)))
    (should (string-prefix-p
             message (error-message-string
                      (should-error (wasm-rootstack-compose :owners owners :full t)))))))

(defun wasm-rootstack-full--reject-access (body message)
  "Append BODY in memory and assert its specific rejection MESSAGE."
  (wasm-rootstack-full--reject-definitions (list `(defun audit_mutation (n) ,body)) message))

(defun wasm-rootstack-full--mutate (function old new message)
  "Replace OLD with NEW inside genuine FUNCTION and assert compose MESSAGE."
  (let* ((owners (plist-get (wasm-rootstack-authority) :owners))
         (original (cl-find function (cdr nelisp-cc-rootstack--source) :key #'cadr))
         (changed (cl-subst new old (copy-tree original) :test #'equal))
         (nelisp-cc-rootstack--source
          (cl-subst changed original (copy-tree nelisp-cc-rootstack--source) :test #'equal)))
    (should original)
    (should-not (equal original changed))
    (should (string-match-p
             message (error-message-string
                      (should-error (wasm-rootstack-compose :owners owners :full t)))))))

(ert-deftest wasm-rootstack/r4-f1-constant-arithmetic ()
  (dolist (body '((ptr-write-u64 (data-addr nl_root_pin_control) (+ 32 32) 1)
                  (ptr-write-u64 (+ (data-addr nl_root_pin_control) (+ 32 32)) 0 1)))
    (wasm-rootstack-full--reject-access
     body "Rootstack usage coverage: nl_root_pin_control size=64 requires=72")))

(ert-deftest wasm-rootstack/r4-f1-let-constant-is-escape ()
  (wasm-rootstack-full--reject-access
   '(let ((n 64)) (ptr-write-u64 (data-addr nl_root_pin_control) n 1))
   "Unreviewed owner escape: nl_root_pin_control audit_mutation (ptr-write-u64"))

(ert-deftest wasm-rootstack/r4-f1-setq-is-escape ()
  (dolist (body '((let ((p (data-addr nl_root_pin_control)))
                   (seq (setq p (+ p 64)) (ptr-write-u64 p 0 1)))
                  (let ((p (data-addr nl_root_pin_control)))
                    (seq (if n (setq p n) 0) (ptr-write-u64 p 0 1)))))
    (wasm-rootstack-full--reject-access
     body "Unreviewed owner escape: nl_root_pin_control audit_mutation (let")))

(ert-deftest wasm-rootstack/r4-f1-nested-return-is-escape ()
  (wasm-rootstack-full--reject-access
   '(let ((p (let ((b (data-addr nl_root_pin_control))) (+ b 64)))) (ptr-write-u64 p 0 1))
   "Unreviewed owner escape: nl_root_pin_control audit_mutation (let"))

(ert-deftest wasm-rootstack/r4-f1-variable-offset-is-escape ()
  (wasm-rootstack-full--reject-access
   '(ptr-write-u64 (+ (data-addr nl_root_pin_control) n) 0 1)
   "Unreviewed owner escape: nl_root_pin_control audit_mutation (+"))

(ert-deftest wasm-rootstack/r4-f1-negative-offset ()
  (wasm-rootstack-full--reject-access
   '(ptr-write-u64 (data-addr nl_root_pin_control) -8 1)
   "Rootstack usage coverage: nl_root_pin_control negative offset=-8 function=audit_mutation"))

(ert-deftest wasm-rootstack/r4-f1-original-rejections ()
  (dolist (body '((ptr-write-u64 (+ (data-addr nl_root_pin_control) 64) 0 1)
                  (ptr-write-u64 (+ 64 (data-addr nl_root_pin_control)) 0 1)
                  (atomic-fetch-add (+ 64 (data-addr nl_root_pin_control)) 1)))
    (wasm-rootstack-full--reject-access
     body "Rootstack usage coverage: nl_root_pin_control size=64 requires=72")))

(ert-deftest wasm-rootstack/r4-f1-genuine-helper-extent ()
  (wasm-rootstack-full--mutate
   'nl_root_pin_v2_commit_token '(+ control 40) '(+ control 64)
   "Rootstack usage coverage: nl_root_pin_control size=64 requires=72"))

(ert-deftest wasm-rootstack/r4-f2-genuine-helper-capacity ()
  (wasm-rootstack-full--mutate
   'nl_root_pin_end_valid 524288 1048576
   "Genuine pin region capacity mismatch:.*function=nl_root_pin_end_valid"))

(ert-deftest wasm-rootstack/r4-f2-in-bounds-field ()
  (let* ((owners (plist-get (wasm-rootstack-authority) :owners))
         (nelisp-cc-rootstack--source
          (append (copy-tree nelisp-cc-rootstack--source)
                  '((defun field_access () (ptr-read-u64 (+ (data-addr nl_root_pin_region) 8) 0))))))
    (should (wasm-rootstack-compose :owners owners :full t))))

(ert-deftest wasm-rootstack/r4-capacity-arithmetic-and-lower-bound ()
  (let* ((source '(defun bound ()
                    (let ((base (data-addr nl_root_pin_region)))
                      (seq (if (< p (+ base 32)) 0 1)
                           (if (> p (+ base (* 16384 32))) 0 1)))))
         (usage (wasm-rootstack--usage source)))
    (should (= (length (plist-get usage :pin-capacities)) 1))
    (should (= (plist-get (car (plist-get usage :pin-capacities)) :size) 524288))
    (should (string-prefix-p
             "Unreviewed owner escape: nl_root_pin_region bound"
             (error-message-string
              (should-error (wasm-rootstack--check-coverage
                             '((:name nl_root_pin_region :size 524288)) source))))))
  (dolist (comparison '((<= p (+ base 64)) (< p (+ base 64))
                        (>= (+ base 64) p) (> (+ base 64) p)
                        (= p (+ base 64)) (/= p (+ base 64))))
    (let ((source `(defun bound () (let ((base (data-addr nl_root_pin_region)))
                                    (if ,comparison 1 0)))))
      (should (= (plist-get (car (plist-get (wasm-rootstack--usage source) :pin-capacities)) :size) 64))
      (should (string-prefix-p
               "Unreviewed owner escape: nl_root_pin_region bound"
               (error-message-string
                (should-error (wasm-rootstack--check-coverage
                               '((:name nl_root_pin_region :size 64)) source))))))))

(ert-deftest wasm-rootstack/r4-r1-mutual-recursion ()
  (wasm-rootstack-full--reject-definitions
   '((defun audit_a (n) (if n (data-addr nl_root_pin_control) (audit_b n)))
     (defun audit_b (n) (audit_a n))
     (defun audit_caller (n) (seq (audit_a n) (ptr-write-u64 (+ (audit_b n) 64) 0 1))))
   "Unreviewed owner escape: nl_root_pin_control audit_a"))

(ert-deftest wasm-rootstack/r4-r1-deep-helpers ()
  (let ((definitions
         (cl-loop for i from 0 below 34 collect
                  `(defun ,(intern (format "audit_deep_%d" i)) ()
                     ,(if (= i 33) '(data-addr nl_root_pin_control)
                        (list (intern (format "audit_deep_%d" (1+ i)))))))))
    (wasm-rootstack-full--reject-definitions
     (append definitions '((defun audit_caller () (ptr-write-u64 (+ (audit_deep_0) 64) 0 1))))
     "Unreviewed owner escape: nl_root_pin_control audit_deep_32")))

(ert-deftest wasm-rootstack/r4-r2-zero-iteration-loop ()
  (wasm-rootstack-full--reject-access
   '(let ((p (+ (data-addr nl_root_pin_control) 64)))
      (while n (setq p (data-addr nl_root_pin_control))) (ptr-write-u64 p 0 1))
   "Unreviewed owner escape: nl_root_pin_control audit_mutation"))

(ert-deftest wasm-rootstack/r4-r2-loop-chain ()
  (wasm-rootstack-full--reject-access
   '(let ((p0 (data-addr nl_root_pin_control)) (p1 (alloc-bytes 72 8))
          (p2 (alloc-bytes 72 8)) (p3 (alloc-bytes 72 8)))
      (while n (ptr-write-u64 p3 64 1) (setq p3 p2 p2 p1 p1 p0 n (- n 1))) 0)
   "Unreviewed owner escape: nl_root_pin_control audit_mutation (setq"))

(ert-deftest wasm-rootstack/r4-r3-shadowing ()
  (wasm-rootstack-full--reject-access
   '(let ((p (+ (data-addr nl_root_pin_control) 64)))
      (let ((p (data-addr nl_root_pin_control))) (if n 0 0)) (ptr-write-u64 p 0 1))
   "Rootstack usage coverage: nl_root_pin_control size=64 requires=72")
  ;; Inner assignments do not mutate the outer binding identity.
  (let ((usage (wasm-rootstack--usage
                '(defun shadow () (let ((p (data-addr nl_root_pin_control)))
                                    (let ((p 0)) (setq p 1)) (ptr-read-u64 p 0))))))
    (should-not (plist-get usage :escapes))
    (should (= (cdr (assq 'nl_root_pin_control (plist-get usage :extents))) 8))))

(ert-deftest wasm-rootstack/r4-r4-cas-store ()
  (wasm-rootstack-full--reject-access
   '(seq (atomic-compare-exchange (data-addr nl_thread_parallel_ctx) 0 (data-addr nl_root_pin_control))
         (ptr-write-u64 (ptr-read-u64 (data-addr nl_thread_parallel_ctx) 0) 64 1))
   "Unreviewed owner escape: nl_root_pin_control audit_mutation (atomic-compare-exchange"))

(ert-deftest wasm-rootstack/r4-r4-heap-store ()
  (wasm-rootstack-full--reject-access
   '(let ((p (alloc-bytes 8 8)))
      (seq (ptr-write-u64 p 0 (data-addr nl_root_pin_control))
           (ptr-write-u64 (ptr-read-u64 p 0) 64 1)))
   "Unreviewed owner escape: nl_root_pin_control audit_mutation (ptr-write-u64"))

(ert-deftest wasm-rootstack/r4-r4-array-aliased-store ()
  (wasm-rootstack-full--reject-access
   '(seq (ptr-write-u64 (data-addr nl_rootstack_region) n (data-addr nl_root_pin_control))
         (ptr-write-u64 (+ (ptr-read-u64 (data-addr nl_rootstack_region) 0) 64) 0 1))
   "Unreviewed owner escape: nl_root_pin_control audit_mutation (ptr-write-u64"))

(ert-deftest wasm-rootstack/r4-r5-registry-second-write ()
  (wasm-rootstack-full--mutate
   'nl_thread_registry_add_at
   '(ptr-write-u64 (nl_thread_registry_entry i) 8 (ptr-read-u64 env 120))
   '(ptr-write-u64 (nl_thread_registry_entry i) 16 (ptr-read-u64 env 120))
   "Unreviewed owner escape: nl_thread_registry nl_thread_registry_add_at (ptr-write-u64"))

(ert-deftest wasm-rootstack/r4-r5-registry-stride ()
  (wasm-rootstack-full--mutate
   'nl_thread_registry_entry '(* i 16) '(* i 32)
   "Unreviewed owner escape: nl_thread_registry nl_thread_registry_entry (+"))

(ert-deftest wasm-rootstack/r4-r6-wrapped-capacity ()
  (wasm-rootstack-full--mutate
   'nl_root_pin_end_valid '(> top (+ base 524288)) '(= (> top (+ base 1048576)) 1)
   "Genuine pin region capacity mismatch:.*function=nl_root_pin_end_valid"))

(ert-deftest wasm-rootstack/r4-edit-genuine-escape ()
  (wasm-rootstack-full--mutate
   'nl_root_pin_reserve '(- top base) '(+ top base)
   "Unreviewed owner escape: nl_root_pin_region nl_root_pin_reserve"))

(ert-deftest wasm-rootstack/r4-stale-pin ()
  (let* ((owners (plist-get (wasm-rootstack-authority) :owners))
         (nelisp-cc-rootstack--source
          (cons 'seq (cl-remove 'nl_root_pin_end_valid (cdr nelisp-cc-rootstack--source) :key #'cadr))))
    (should (string-prefix-p
             "Stale pinned owner escape: nl_root_pin_region nl_root_pin_end_valid"
             (error-message-string (should-error (wasm-rootstack-compose :owners owners :full t)))))))

(ert-deftest wasm-rootstack/r4-pinned-set-and-classes ()
  (let* ((authority (wasm-rootstack-authority)) (usage (plist-get authority :usage)))
    (should (equal (wasm-rootstack--pinned-escapes) (plist-get usage :escapes)))
    (should (equal (plist-get authority :escapes-sha256) (wasm-rootstack--hash wasm-rootstack--escapes-file)))
    (should (= (length (plist-get usage :classifications))
               (wasm-rootstack--literal-count nelisp-cc-rootstack--source)))
    (dolist (entry (plist-get usage :classifications)) (should (memq (nth 2 entry) '(direct escape))))
    (should (memq 'nl_thread_registry wasm-rootstack--array-owners))
    (should (wasm-rootstack-compose :owners (plist-get authority :owners) :full t))))

(ert-deftest wasm-rootstack/r4-array-constant-extent ()
  (wasm-rootstack-full--reject-access
   '(ptr-write-u64 (data-addr nl_thread_registry) 1040 1)
   "Rootstack usage coverage: nl_thread_registry size=1040 requires=1048"))

(ert-deftest wasm-rootstack/r4-depth-one-recursion ()
  (let* ((source '(seq (defun recursive (p) (seq (ptr-read-u64 p 0) (recursive p)))
                       (defun caller () (recursive (data-addr nl_root_pin_control)))))
         (usage (wasm-rootstack--usage source)))
    (should (= (cdr (assq 'nl_root_pin_control (plist-get usage :extents))) 8))
    (should (member '(nl_root_pin_control recursive (recursive p)) (plist-get usage :escapes)))
    (should (member '(nl_root_pin_control caller (recursive (data-addr nl_root_pin_control)))
                    (plist-get usage :escapes)))))

(ert-deftest wasm-rootstack/r4-alias-copy-is-escape ()
  (wasm-rootstack-full--reject-access
   '(let* ((p (data-addr nl_root_pin_control)) (q p)) (ptr-read-u64 q 0))
   "Unreviewed owner escape: nl_root_pin_control audit_mutation (q p)"))

(ert-deftest wasm-rootstack/r4-boolean-assignment-escapes ()
  (dolist (operator '(and or))
    (wasm-rootstack-full--reject-access
     `(let ((p (data-addr nl_root_pin_control)))
        (seq (,operator n (setq p (+ p 64))) (ptr-read-u64 p 0)))
     "Unreviewed owner escape: nl_root_pin_control audit_mutation (let")))

(ert-deftest wasm-rootstack/r4-fixed-cell-and-external-escapes ()
  (wasm-rootstack-full--reject-access
   '(seq (ptr-write-u64 (data-addr nl_thread_parallel_ctx) 0 (data-addr nl_root_pin_control))
         (ptr-write-u64 (ptr-read-u64 (data-addr nl_thread_parallel_ctx) 0) 64 1))
   "Unreviewed owner escape: nl_root_pin_control audit_mutation (ptr-write-u64")
  (dolist (owner '(nl_root_pin_control nl_thread_registry))
    (wasm-rootstack-full--reject-access
     `(opaque_external (data-addr ,owner))
     (format "Unreviewed owner escape: %s audit_mutation (opaque_external" owner))))

(ert-deftest wasm-rootstack/r4-same-argument-scalar-recursion ()
  (let* ((owners (plist-get (wasm-rootstack-authority) :owners))
         (nelisp-cc-rootstack--source
          (append nelisp-cc-rootstack--source
                  '((defun retry_store (value)
                      (if (= (atomic-compare-exchange
                              (+ (data-addr nl_thread_parallel_ctx) 24)
                              (atomic-fetch-add (+ (data-addr nl_thread_parallel_ctx) 24) 0) value) 1)
                          value (retry_store value)))
                    (defun caller () (opaque_external (retry_store 0)))))))
    (should (wasm-rootstack-compose :owners owners :full t))))

(ert-deftest wasm-rootstack/r4-unresolved-helper-bound ()
  (wasm-rootstack-full--mutate
   'nl_root_pin_end_valid '(+ base 524288) '(opaque-bound base)
   "Unreviewed owner escape: nl_root_pin_region nl_root_pin_end_valid (opaque-bound base)"))

(ert-deftest wasm-rootstack/r4-zero-and-wrong-capacity ()
  (dolist (size '(0 -8 1048576))
    (wasm-rootstack-full--mutate
     'nl_root_pin_reserve 524288 size
     "Genuine pin region capacity mismatch:.*function=nl_root_pin_reserve"))
  (should (string-prefix-p
           "Genuine pin region capacity mismatch:"
           (error-message-string
            (should-error
             (wasm-rootstack--check-coverage
              '((:name nl_root_pin_region :size 524288))
              '(defun bound () (let ((base (data-addr nl_root_pin_region)))
                                (if (> p (+ base (* 16384 16))) 0 1)))))))))

(ert-deftest wasm-rootstack/r4-bounded-probe-printer ()
  (let* ((authority (wasm-rootstack-authority))
         (expected (plist-get (plist-get authority :usage) :escapes))
         (print-length 6) (print-level 3))
    (should (equal expected (wasm-rootstack--pinned-escapes)))
    (should (equal expected (plist-get (wasm-rootstack--usage nelisp-cc-rootstack--source) :escapes)))
    (should (wasm-rootstack-compose :owners (plist-get authority :owners) :full t))))

(ert-deftest wasm-rootstack/r4-binding-declarations-are-not-calls ()
  (let ((usage (wasm-rootstack--usage
                '(defun named_binding ()
                   (let ((+ (data-addr nl_root_pin_control))) (ptr-read-u64 + 0))))))
    (should-not (plist-get usage :escapes))
    (should (= (cdr (assq 'nl_root_pin_control (plist-get usage :extents))) 8)))
  (wasm-rootstack-full--reject-definitions
   '((defun q (p) (ptr-read-u64 p 64))
     (defun audit_mutation () (let* ((p (data-addr nl_root_pin_control)) (q p))
                               (ptr-read-u64 q 0))))
   "Unreviewed owner escape: nl_root_pin_control audit_mutation (q p)"))

(ert-deftest wasm-rootstack/r5-f1-cond-write ()
  (wasm-rootstack-full--reject-access
   '(cond ((ptr-write-u64 (data-addr nl_root_pin_control) 64 1) 1) (t 0))
   "Rootstack usage coverage: nl_root_pin_control size=64 requires=72"))

(ert-deftest wasm-rootstack/r5-f1-cond-opaque ()
  (wasm-rootstack-full--reject-access
   '(cond ((opaque_external (data-addr nl_root_pin_control)) 1) (t 0))
   "Unreviewed owner escape: nl_root_pin_control audit_mutation (opaque_external (data-addr nl_root_pin_control))"))

(ert-deftest wasm-rootstack/r5-f2-atomic-helper ()
  (wasm-rootstack-full--reject-definitions
   '((defun atomic-audit (p) (ptr-write-u64 p 64 1))
     (defun audit_mutation () (atomic-audit (data-addr nl_root_pin_control))))
   "Rootstack usage coverage: nl_root_pin_control size=64 requires=72")
  (wasm-rootstack-full--reject-access
   '(atomic-audit (data-addr nl_root_pin_control))
   "Unreviewed owner escape: nl_root_pin_control audit_mutation (atomic-audit (data-addr nl_root_pin_control))"))

(ert-deftest wasm-rootstack/r5-f3-comparison-escapes ()
  (dolist (body '((= (+ (data-addr nl_root_pin_control) 64) 0)
                  (let ((p (data-addr nl_root_pin_control))) (= (+ p 64) 0))))
    (wasm-rootstack-full--reject-access
     body "Unreviewed owner escape: nl_root_pin_control audit_mutation (=")))

(ert-deftest wasm-rootstack/r5-f4-generator-prologue ()
  (let ((reader (symbol-function 'wasm-rootstack--build-definition)))
    ;; Exercise the structural gate independently of the complete-definition pin.
    (cl-letf (((symbol-function 'wasm-rootstack--check-reviewed-definition)
               (lambda (_name _definition) nil))
              ((symbol-function 'wasm-rootstack--build-definition)
               (lambda (kind name)
                 (let ((definition (funcall reader kind name)))
                   (if (eq name 'nelisp-standalone--root-pin-region-offset)
                       (append (cl-subseq definition 0 3)
                               '((setq nelisp-standalone--root-pin-slots 1))
                               (nthcdr 3 definition))
                     definition)))))
      (should (string-prefix-p
               "Unresolved ownership generator: definition shape nelisp-standalone--root-pin-region-offset"
               (error-message-string (should-error (wasm-rootstack-authority))))))))

(ert-deftest wasm-rootstack/r5-f4-conditional-declaration ()
  (let ((reader (symbol-function 'wasm-rootstack--read-definition)))
    ;; Exercise the structural gate independently of the complete-definition pin.
    (cl-letf (((symbol-function 'wasm-rootstack--check-reviewed-definition)
               (lambda (_name _definition) nil))
              ((symbol-function 'wasm-rootstack--read-definition)
               (lambda (file kind name)
                 (let ((definition (funcall reader file kind name)))
                   (if (not (eq name 'nelisp-standalone--arena-base-slot-unit)) definition
                     (let* ((original (cl-find-if
                                       (lambda (x) (and (eq (car-safe x) 'nelisp-link-symbol)
                                                        (equal (cadr x) "nl_root_pin_region")))
                                       (wasm-rootstack--forms definition (lambda (_) t))))
                            (other (copy-tree original)))
                       (setf (nth 2 other) '(+ (nelisp-standalone--root-pin-region-offset) 32))
                       (cl-subst `(if (nelisp-standalone--runtime-reload-enabled-p)
                                      ,original ,other)
                                 original definition :test #'equal)))))))
      (should (string-prefix-p
               "Unsupported ownership declaration context: nl_root_pin_region"
               (error-message-string (should-error (wasm-rootstack-authority))))))))

(ert-deftest wasm-rootstack/r5-f1-executable-positions ()
  (let ((write '(ptr-write-u64 (data-addr nl_root_pin_control) 64 1)))
    (dolist (body `((let ((p ,write)) p) (let* ((p ,write)) p)
                    (let () ,write) (let* () ,write)
                    (if ,write 0 1) (if 0 ,write 1) (if 1 0 ,write)
                    (cond (,write 1) (t 0)) (cond (t ,write))
                    (while ,write 0) (while 0 ,write)
                    (seq ,write 0) (progn 0 ,write)
                    (setq n ,write) (and ,write 1) (or 0 ,write)
                    (catch ,write 0) (catch 'tag ,write)
                    (throw ,write 0) (throw 'tag ,write)))
      (wasm-rootstack-full--reject-access
       body "Rootstack usage coverage: nl_root_pin_control size=64 requires=72"))))

(ert-deftest wasm-rootstack/r5-f1-unsupported-positions ()
  (dolist (body '((quote (data-addr nl_root_pin_control))
                  (function (lambda () (data-addr nl_root_pin_control)))
                  ((lambda () (data-addr nl_root_pin_control)))
                  (let ((p 0 (data-addr nl_root_pin_control))) p)
                  (condition-case err (data-addr nl_root_pin_control) (error 0))))
    (wasm-rootstack-full--reject-access body "Unsupported owner syntax:")))

(ert-deftest wasm-rootstack/r5-f1-independent-inventory ()
  (let* ((source '(seq (defun audit_mutation ()
                        (cond ((ptr-read-u64 (data-addr nl_root_pin_control) 0)
                               (ptr-read-u64 (data-addr nl_root_pin_control) 8))
                              (t (ptr-read-u64 (data-addr nl_root_pin_control) 16))))))
         (usage (wasm-rootstack--usage source)))
    (should (= (wasm-rootstack--literal-count source) 3))
    (should (= (length (plist-get usage :classifications))
               (wasm-rootstack--literal-count source))))
  ;; Break the classifier output alone: compose must catch the independent count.
  (let ((usage-reader (symbol-function 'wasm-rootstack--usage))
        (owners (plist-get (wasm-rootstack-authority) :owners)))
    (cl-letf (((symbol-function 'wasm-rootstack--usage)
               (lambda (source)
                 (let ((usage (funcall usage-reader source)))
                   (plist-put usage :classifications
                              (cdr (plist-get usage :classifications)))))))
      (should (equal
               "Unsupported owner syntax: literal inventory=73 classified=72"
               (error-message-string
                (should-error (wasm-rootstack-compose :owners owners :full t))))))))

(ert-deftest wasm-rootstack/r5-f3-capacity-independent-of-pins ()
  (dolist (body '((if (> n (+ (data-addr nl_root_pin_region) 1048576)) 0 1)
                  (let ((base (data-addr nl_root_pin_region)))
                    (if (> n (+ base 1048576)) 0 1))
                  (let ((base (data-addr nl_root_pin_region)))
                    (cond ((> n (+ base 1048576)) 0) (t 1)))))
    (wasm-rootstack-full--reject-access body "Genuine pin region capacity mismatch:"))
  (wasm-rootstack-full--reject-definitions
   '((defun audit_bound (base n) (if (> n (+ base 1048576)) 0 1))
     (defun audit_mutation (n) (audit_bound (data-addr nl_root_pin_region) n)))
   "Genuine pin region capacity mismatch:"))

(ert-deftest wasm-rootstack/r5-f4-complete-generator-shapes ()
  (let ((reader (symbol-function 'wasm-rootstack--build-definition)))
    (dolist (name '(nelisp-standalone--driver-bss-base-size
                    nelisp-standalone--root-pin-region-offset
                    nelisp-standalone--root-pin-slots))
      (dolist (change '(name kind args-or-value prologue epilogue duplicate-docstring))
        ;; Exercise the structural gate independently of the complete-definition pin.
	(cl-letf (((symbol-function 'wasm-rootstack--check-reviewed-definition)
                   (lambda (_name _definition) nil))
                  ((symbol-function 'wasm-rootstack--build-definition)
                   (lambda (kind requested)
                     (let ((definition (copy-tree (funcall reader kind requested))))
                       (if (not (eq requested name)) definition
                         (pcase change
                           ('name (setf (nth 1 definition) 'changed) definition)
                           ('kind (setf (nth 0 definition) 'changed) definition)
                           ('args-or-value
                            (if (eq kind 'defun)
                                (progn (setf (nth 2 definition) '(changed)) definition)
                              (list 'defconst name)))
                           ('prologue (append (cl-subseq definition 0 3)
                                              '((setq changed 1)) (nthcdr 3 definition)))
                           ('epilogue (append definition '((setq changed 1))))
                           ('duplicate-docstring (append definition '("extra docstring")))))))))
          (should (string-prefix-p
                   (format "Unresolved ownership generator: definition shape %s" name)
                   (error-message-string (should-error (wasm-rootstack-authority))))))))))

(ert-deftest wasm-rootstack/r5-f4-boundary-count-and-context ()
  (let ((reader (symbol-function 'wasm-rootstack--read-definition)))
    (dolist (change '(duplicate missing opaque-wrapper quoted))
      ;; Exercise the structural gate independently of the complete-definition pin.
      (cl-letf (((symbol-function 'wasm-rootstack--check-reviewed-definition)
                 (lambda (_name _definition) nil))
                ((symbol-function 'wasm-rootstack--read-definition)
                 (lambda (file kind name)
                   (let ((definition (funcall reader file kind name)))
                     (if (not (eq name 'nelisp-standalone--arena-base-slot-unit)) definition
                       (let ((original (cl-find-if
                                        (lambda (x) (and (eq (car-safe x) 'nelisp-link-symbol)
                                                         (equal (cadr x) "nl_root_pin_region")))
                                        (wasm-rootstack--forms definition (lambda (_) t)))))
                         (cl-subst (pcase change
                                     ('duplicate `(list ,original ,original))
                                     ('missing nil)
                                     ('opaque-wrapper `(opaque ,original))
                                     ('quoted `(quote ,original)))
                                   original definition :test #'equal)))))))
        (should (string-prefix-p
                 (pcase change
                   ('duplicate "Ownership boundary declaration count: nl_root_pin_region count=2")
                   ('missing "Ownership boundary declaration count: nl_root_pin_region count=0")
                   (_ "Unsupported ownership declaration context: nl_root_pin_region"))
                 (error-message-string (should-error (wasm-rootstack-authority)))))))))


(ert-deftest wasm-rootstack/r6-f1-source-constant-grammar ()
  (should (= (wasm-rootstack--constant '(logand (+ (* 3 8) (- 40 9)) -32)) 32))
  (should (= (wasm-rootstack--constant '(- 8)) -8))
  (dolist (form '(nelisp-standalone--root-pin-slots n
                  (+ nelisp-standalone--root-pin-slots -16384)
                  (nelisp-standalone--driver-bss-base-size)
                  (nelisp-standalone--runtime-reload-enabled-p)
                  (if (nelisp-standalone--runtime-reload-enabled-p) 64 0)))
    (should-not (wasm-rootstack--constant form))))

(ert-deftest wasm-rootstack/r6-f1-shadow-native-constant ()
  (wasm-rootstack-full--reject-definitions
   '((defun audit_mutation ()
       (let ((nelisp-standalone--root-pin-slots 16448))
         (ptr-write-u64 (data-addr nl_root_pin_control)
                        (+ nelisp-standalone--root-pin-slots -16384) 1))))
   "Unreviewed owner escape: nl_root_pin_control audit_mutation (ptr-write-u64 (data-addr nl_root_pin_control) (+ nelisp-standalone--root-pin-slots -16384) 1)"))

(ert-deftest wasm-rootstack/r6-f1-shadow-native-predicate ()
  (wasm-rootstack-full--reject-definitions
   '((defun nelisp-standalone--runtime-reload-enabled-p () 1)
     (defun audit_mutation ()
       (ptr-write-u64 (data-addr nl_root_pin_control)
                      (if (nelisp-standalone--runtime-reload-enabled-p) 64 0) 1)))
   "Unreviewed owner escape: nl_root_pin_control audit_mutation (ptr-write-u64 (data-addr nl_root_pin_control) (if (nelisp-standalone--runtime-reload-enabled-p) 64 0) 1)"))

(ert-deftest wasm-rootstack/r6-f2-enclosing-generator-prologue ()
  (let ((reader (symbol-function 'wasm-rootstack--read-definition)))
    (cl-letf (((symbol-function 'wasm-rootstack--read-definition)
               (lambda (file kind name)
                 (let ((definition (funcall reader file kind name)))
                   (if (eq name 'nelisp-standalone--arena-base-slot-unit)
                       (append (cl-subseq definition 0 3)
                               '((setq nelisp-standalone--root-pin-slots 1))
                               (nthcdr 3 definition))
                     definition)))))
      (should (equal
               "Unreviewed ownership generator change: nelisp-standalone--arena-base-slot-unit"
               (error-message-string (should-error (wasm-rootstack-authority))))))))

(ert-deftest wasm-rootstack/r6-f2-all-reviewed-definitions ()
  ;; Change only the docstring: every structural gate still accepts this shape.
  (let ((reader (symbol-function 'wasm-rootstack--read-definition)))
    (dolist (name '(nelisp-standalone--arena-base-slot-unit
                    nelisp-standalone--driver-bss-base-size
                    nelisp-standalone--root-pin-region-offset
                    nelisp-standalone--root-pin-slots
                    nelisp-standalone--runtime-reload-enabled-p
                    nelisp-standalone--callsite-enabled-p))
      (cl-letf (((symbol-function 'wasm-rootstack--read-definition)
                 (lambda (file kind requested)
                   (let ((definition (copy-tree (funcall reader file kind requested))))
                     (when (eq requested name)
                       (if (stringp (nth 3 definition))
                           (setf (nth 3 definition) "Unreviewed docstring")
                         (setq definition (append (cl-subseq definition 0 3)
                                                  '("Unreviewed docstring")
                                                  (nthcdr 3 definition)))))
                     definition))))
        (should (equal
                 (format "Unreviewed ownership generator change: %s" name)
                 (error-message-string (should-error (wasm-rootstack-authority)))))))))

(ert-deftest wasm-rootstack/r6-f3-unused-owner-alias ()
  (wasm-rootstack-full--reject-definitions
   '((defun audit_mutation ()
       (let ((p (+ (data-addr nl_root_pin_control) 64))) 0)))
   "Unreviewed owner escape: nl_root_pin_control audit_mutation (p (+ (data-addr nl_root_pin_control) 64))"))

(ert-deftest wasm-rootstack/r6-f3-unconsumed-initializers ()
  (dolist (body '((let ((p (data-addr nl_root_pin_control))) 0)
                  (let* ((p (data-addr nl_root_pin_control)) (p 0)) p)))
    (wasm-rootstack-full--reject-access
     body "Unreviewed owner escape: nl_root_pin_control audit_mutation (p (data-addr nl_root_pin_control))"))
  ;; A nested binding uses the initializer in an unchecked position: still escape.
  (wasm-rootstack-full--reject-access
   '(let ((p (data-addr nl_root_pin_control))) (let ((q p)) 0))
   "Unreviewed owner escape: nl_root_pin_control audit_mutation (q p)"))


(ert-deftest wasm-rootstack/r6-f2-pins-precede-structural-gates ()
  (let ((reader (symbol-function 'wasm-rootstack--read-definition)))
    (dolist (name '(nelisp-standalone--arena-base-slot-unit
                    nelisp-standalone--driver-bss-base-size
                    nelisp-standalone--root-pin-region-offset
                    nelisp-standalone--root-pin-slots
                    nelisp-standalone--runtime-reload-enabled-p
                    nelisp-standalone--callsite-enabled-p))
      (cl-letf (((symbol-function 'wasm-rootstack--read-definition)
                 (lambda (file kind requested)
                   (let ((definition (funcall reader file kind requested)))
                     (if (eq requested name)
                         (append (cl-subseq definition 0 3) '((setq changed 1))
                                 (nthcdr 3 definition))
                       definition)))))
        (should (equal
                 (format "Unreviewed ownership generator change: %s" name)
                 (error-message-string (should-error (wasm-rootstack-authority)))))))))

(ert-deftest wasm-rootstack/r6-f2-unlimited-printer ()
  (let* ((spec (car wasm-rootstack--reviewed-generators))
         (definition (wasm-rootstack--build-definition (car spec) (cdr spec)))
         (expected (wasm-rootstack--check-reviewed-definition (cdr spec) definition))
         (print-length 1) (print-level 1))
    (should (equal expected
                   (wasm-rootstack--check-reviewed-definition (cdr spec) definition)))))


(ert-deftest wasm-rootstack/r6-f1-native-symbol-capacity-is-escape ()
  (wasm-rootstack-full--reject-access
   '(let ((base (data-addr nl_root_pin_region)))
      (if (> n (+ base (* nelisp-standalone--root-pin-slots 32))) 0 1))
   "Unreviewed owner escape: nl_root_pin_region audit_mutation (+ base (* nelisp-standalone--root-pin-slots 32))"))

(defun wasm-rootstack-full--reject-build-copy (kind name replacement message &optional replace)
  "Check MESSAGE using a real build copy with REPLACEMENT of KIND/NAME.
REPLACEMENT maps the original read form to forms appended at EOF, or replacing
the original when REPLACE is non-nil.  Never mock the authority reader."
  (let* ((file (expand-file-name "scripts/nelisp-standalone-build.el" wasm-rootstack--root))
         (original (wasm-rootstack--read-definition file kind name))
         (directory (expand-file-name "target/wasm-rootstack/" wasm-rootstack--root))
         (temporary (progn (make-directory directory t)
                           (make-temp-file (expand-file-name "r7-build-" directory) t))))
    (unwind-protect
        (let ((copy (expand-file-name "scripts/nelisp-standalone-build.el" temporary)))
          (make-directory (file-name-directory copy) t)
          (copy-file file copy)
          (with-temp-buffer
            (insert-file-contents copy)
            (if replace
                (let (found)
                  (goto-char (point-min))
                  (while (not found)
                    (let* ((start (point)) (form (read (current-buffer))))
                      (when (and (eq (car-safe form) kind) (eq (cadr form) name))
                        (delete-region start (point))
                        (setq found t)))))
              (goto-char (point-max)))
            (insert "\n")
            (dolist (form (funcall replacement (copy-tree original)))
              (let ((print-length nil) (print-level nil)) (prin1 form (current-buffer)))
              (insert "\n"))
            (write-region (point-min) (point-max) copy nil 'silent))
          (let ((wasm-rootstack--root temporary))
            (should (equal message
                           (error-message-string (should-error (wasm-rootstack-authority)))))))
      (delete-directory temporary t))))

(ert-deftest wasm-rootstack/r7-f1-p1-later-redefinition ()
  (wasm-rootstack-full--reject-build-copy
   'defun 'nelisp-standalone--root-pin-region-offset
   (lambda (form) (list (cl-subst 32 64 form)))
   "Unreviewed ownership generator change: nelisp-standalone--root-pin-region-offset (definition context)"))

(ert-deftest wasm-rootstack/r7-f1-p2-later-defconst ()
  (wasm-rootstack-full--reject-build-copy
   'defconst 'nelisp-standalone--root-pin-slots
   (lambda (form) (list (cl-subst 8192 16384 form)))
   "Unreviewed ownership generator change: nelisp-standalone--root-pin-slots (definition context)"))

(ert-deftest wasm-rootstack/r7-f1-p3-top-level-setq ()
  (wasm-rootstack-full--reject-build-copy
   'defconst 'nelisp-standalone--root-pin-slots
   (lambda (_) '((setq unrelated 0 nelisp-standalone--root-pin-slots 8192)))
   "Unreviewed ownership generator change: nelisp-standalone--root-pin-slots (definition context)"))

(ert-deftest wasm-rootstack/r7-f1-p4-when-nil-original ()
  (wasm-rootstack-full--reject-build-copy
   'defconst 'nelisp-standalone--root-pin-slots
   (lambda (form) (list `(when nil ,form) (cl-subst 8192 16384 form)))
   "Unreviewed ownership generator change: nelisp-standalone--root-pin-slots (definition context)" t))

(ert-deftest wasm-rootstack/r7-f1-p5-quoted-original ()
  (wasm-rootstack-full--reject-build-copy
   'defun 'nelisp-standalone--root-pin-region-offset
   (lambda (form) (list `(defvar audit-unused ',form) (cl-subst 32 64 form)))
   "Unreviewed ownership generator change: nelisp-standalone--root-pin-region-offset (definition context)" t))

(ert-deftest wasm-rootstack/r7-f1-p6-in-place-change ()
  (wasm-rootstack-full--reject-build-copy
   'defconst 'nelisp-standalone--root-pin-slots
   (lambda (form) (list (cl-subst 8192 16384 form)))
   "Unreviewed ownership generator change: nelisp-standalone--root-pin-slots" t))

(ert-deftest wasm-rootstack/r7-f1-all-reviewed-names ()
  (dolist (spec wasm-rootstack--reviewed-generators)
    (wasm-rootstack-full--reject-build-copy
     (car spec) (cdr spec) (lambda (form) (list form))
     (format "Unreviewed ownership generator change: %s (definition context)" (cdr spec)))))

(ert-deftest wasm-rootstack/r7-f1-definition-and-assignment-targets ()
  (dolist (form '((defun nelisp-standalone--root-pin-slots () 0)
                  (defmacro nelisp-standalone--root-pin-slots () 0)
                  (defsubst nelisp-standalone--root-pin-slots () 0)
                  (cl-defun nelisp-standalone--root-pin-slots () 0)
                  (defvar nelisp-standalone--root-pin-slots 0)
                  (defcustom nelisp-standalone--root-pin-slots 0 "Audit")
                  (defalias 'nelisp-standalone--root-pin-slots 'ignore)
                  (fset 'nelisp-standalone--root-pin-slots 'ignore)
                  (set 'nelisp-standalone--root-pin-slots 0)
                  (setf unrelated 0 nelisp-standalone--root-pin-slots 0)
                  (setq-default unrelated 0 nelisp-standalone--root-pin-slots 0)
                  (setq-local unrelated 0 nelisp-standalone--root-pin-slots 0)
                  (setf (symbol-value 'nelisp-standalone--root-pin-slots) 0)))
    (wasm-rootstack-full--reject-build-copy
     'defconst 'nelisp-standalone--root-pin-slots (lambda (_) (list form))
     "Unreviewed ownership generator change: nelisp-standalone--root-pin-slots (definition context)")))

(ert-deftest wasm-rootstack/r7-f1-nested-definition-contexts ()
  ;; A single nested occurrence fails too, even if its printed form is reviewed.
  (dolist (wrapper '((quote BODY) (function BODY) (when nil BODY) (if nil BODY 0)
                     (progn BODY) (eval-when-compile BODY) (let ((x 0)) BODY)
                     [BODY]))
    (wasm-rootstack-full--reject-build-copy
     'defconst 'nelisp-standalone--root-pin-slots
     (lambda (form) (list (cl-subst form 'BODY wrapper)))
     "Unreviewed ownership generator change: nelisp-standalone--root-pin-slots (definition context)" t)))

(ert-deftest wasm-rootstack/r7-f1-missing-definition ()
  (wasm-rootstack-full--reject-build-copy
   'defconst 'nelisp-standalone--root-pin-slots (lambda (_) nil)
   "Unreviewed ownership generator change: nelisp-standalone--root-pin-slots (definition context)" t))

(ert-deftest wasm-rootstack/r7-f2-duplicate-registry-write ()
  (let ((write '(ptr-write-u64 (nl_thread_registry_entry i) 8 (ptr-read-u64 env 120))))
    (wasm-rootstack-full--mutate
     'nl_thread_registry_add_at
     `(if (>= i 64) (- 0 1)
        (seq (ptr-write-u64 (nl_thread_registry_entry i) 0 env)
             ,write (ptr-write-u64 (data-addr nl_thread_registry) 0 (+ i 1)) i))
     `(seq ,write
           (if (>= i 64) (- 0 1)
             (seq (ptr-write-u64 (nl_thread_registry_entry i) 0 env)
                  ,write (ptr-write-u64 (data-addr nl_thread_registry) 0 (+ i 1)) i)))
     (regexp-quote
      "Unreviewed owner escape: nl_thread_registry nl_thread_registry_add_at (ptr-write-u64 (nl_thread_registry_entry i) 8 (ptr-read-u64 env 120))"))))

(ert-deftest wasm-rootstack/r7-f2-duplicate-pin-begin-escape ()
  (let ((write '(ptr-write-u64 (data-addr nl_root_pin_control) 24 (data-addr nl_root_pin_region))))
    (wasm-rootstack-full--mutate
     'nl_root_pin_begin write `(seq ,write ,write)
     (regexp-quote
      "Unreviewed owner escape: nl_root_pin_region nl_root_pin_begin (ptr-write-u64 (data-addr nl_root_pin_control) 24 (data-addr nl_root_pin_region))"))))

(ert-deftest wasm-rootstack/r7-f2-multiset-counts ()
  (let* ((usage (wasm-rootstack--usage nelisp-cc-rootstack--source))
         (pinned (wasm-rootstack--pinned-escapes)))
    (should (= (length pinned) 93))
    (should (equal pinned (plist-get usage :escapes)))
    (dolist (entry '((nl_root_pin_control nl_root_pin_begin_v2 (nl_root_pin_v2_commit_token control token))
                     (nl_root_pin_region nl_root_pin_slot_v2 (< marker base))
                     (nl_root_pin_region nl_root_pin_slot_v2 (> safe-marker base))))
      (should (= (cl-count entry pinned :test #'equal) 2))
      (let ((less (copy-tree usage)))
        (setf (plist-get less :escapes)
              (cl-remove entry (plist-get less :escapes) :test #'equal :count 1))
        (should (equal
                 (format "Stale pinned owner escape: %s %s %S" (nth 0 entry) (nth 1 entry) (nth 2 entry))
                 (error-message-string
                  (should-error (wasm-rootstack--check-escapes less nelisp-cc-rootstack--source t)))))))))

(defun wasm-rootstack-full--with-native-copy (suffix function &optional extra)
  "Call FUNCTION in a real build copy ending in SUFFIX, with optional EXTRA library.
Dependencies are symlinks to the unchanged lane; only the build and EXTRA are
copied.  Neither the native oracle nor the syntax reader is mocked."
  (let* ((root wasm-rootstack--root)
         (temporary (make-temp-file (expand-file-name "target/wasm-rootstack/r8-build-" root) t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "scripts" temporary))
          (make-directory (expand-file-name "lisp" temporary))
          (make-symbolic-link (expand-file-name "src" root) (expand-file-name "src" temporary))
          (dolist (directory '("scripts" "lisp"))
            (dolist (file (directory-files (expand-file-name directory root) t "\\`[^.]"))
              (unless (equal (file-name-nondirectory file) "nelisp-standalone-build.el")
                (make-symbolic-link file (expand-file-name
                                         (concat directory "/" (file-name-nondirectory file)) temporary)))))
          (let ((copy (expand-file-name "scripts/nelisp-standalone-build.el" temporary)))
            (copy-file (expand-file-name "scripts/nelisp-standalone-build.el" root) copy)
            (with-temp-buffer
              (insert "\n" suffix "\n")
              (write-region (point-min) (point-max) copy t 'silent)))
          (when extra
            (with-temp-file (expand-file-name "lisp/audit-redef.el" temporary) (insert extra)))
          (let ((wasm-rootstack--root temporary)) (funcall function temporary)))
      (delete-directory temporary t))))

(defun wasm-rootstack-full--reject-native-copy (suffix message &optional extra)
  "Require the exact authority rejection MESSAGE for SUFFIX and optional EXTRA."
  (wasm-rootstack-full--with-native-copy
   suffix (lambda (_) (should (equal message (error-message-string
                                             (should-error (wasm-rootstack-authority)))))) extra))

(ert-deftest wasm-rootstack/r8-q1-defalias-function-quote ()
  (wasm-rootstack-full--reject-native-copy
   "(defalias #'nelisp-standalone--root-pin-region-offset (lambda () (logand (+ (nelisp-standalone--driver-bss-base-size) 32 31) -32)))"
   "Native layout oracle disagreement: nl_root_pin_control syntactic=(4253824 . 64) native=(4253824 . 32)"))

(ert-deftest wasm-rootstack/r8-q2-set-default ()
  (wasm-rootstack-full--reject-native-copy
   "(set-default 'nelisp-standalone--root-pin-slots 8192)"
   "Native layout oracle disagreement: nl_root_pin_region syntactic=(4253888 . 524288) native=(4253888 . 262144)"))

(ert-deftest wasm-rootstack/r8-q3-advice-add ()
  (wasm-rootstack-full--reject-native-copy
   "(advice-add 'nelisp-standalone--root-pin-region-offset :filter-return (lambda (v) (- v 32)))"
   "Native layout oracle disagreement: nl_root_pin_control syntactic=(4253824 . 64) native=(4253824 . 32)"))

(ert-deftest wasm-rootstack/r8-q4-eval-read-string ()
  (wasm-rootstack-full--reject-native-copy
   "(eval (read \"(setq nelisp-standalone--root-pin-slots 8192)\") t)"
   "Native layout oracle disagreement: nl_root_pin_region syntactic=(4253888 . 524288) native=(4253888 . 262144)"))

(ert-deftest wasm-rootstack/r8-q5-required-file ()
  (wasm-rootstack-full--reject-native-copy
   "(require 'audit-redef)"
   "Native layout oracle disagreement: nl_root_pin_region syntactic=(4253888 . 524288) native=(4253888 . 262144)"
   "(defconst nelisp-standalone--root-pin-slots 8192 \"Audit mutation.\")\n(provide 'audit-redef)\n"))

(ert-deftest wasm-rootstack/r9-t1-target-dependent-advice ()
  ;; The oracle evaluates under the explicit wasm32 target, not Linux.
  (wasm-rootstack-full--reject-native-copy
   "(advice-add 'nelisp-standalone--root-pin-region-offset :filter-return (lambda (v) (if (eq (nelisp-standalone-arena-rewrite-target) 'linux-x86_64) v (- v 32))))"
   "Native layout oracle disagreement: nl_root_pin_control syntactic=(4253824 . 64) native=(4253824 . 32)"))

(ert-deftest wasm-rootstack/r9-t2-stale-elc-prefers-newer-source ()
  ;; A stale .elc matching the reviewed value must not hide a newer .el.
  (wasm-rootstack-full--with-native-copy
   "(require 'audit-redef)"
   (lambda (root)
     (let* ((source (expand-file-name "lisp/audit-redef.el" root))
            (compiled (concat source "c")))
       (with-temp-file source
         (insert ";;; -*- lexical-binding: t; -*-\n"
                 "(defconst nelisp-standalone--root-pin-slots 16384 \"Audit stale.\")\n"
                 "(provide 'audit-redef)\n"))
       (require 'bytecomp)
       (should (byte-compile-file source))
       (set-file-times compiled (time-subtract (current-time) 3600))
       (with-temp-file source
         (insert ";;; -*- lexical-binding: t; -*-\n"
                 "(defconst nelisp-standalone--root-pin-slots 8192 \"Audit newer.\")\n"
                 "(provide 'audit-redef)\n"))
       (should (file-newer-than-file-p source compiled))
       (should (equal "Native layout oracle disagreement: nl_root_pin_region syntactic=(4253888 . 524288) native=(4253888 . 262144)"
                      (error-message-string (should-error (wasm-rootstack-authority)))))))
   "(provide 'audit-redef)\n"))

(ert-deftest wasm-rootstack/r9-t3-newer-build-elc ()
  ;; A newer compiled build script shadows the reviewed .el in native loads.
  (wasm-rootstack-full--with-native-copy
   ""
   (lambda (root)
     (require 'bytecomp)
     (let* ((build (expand-file-name "scripts/nelisp-standalone-build.el" root))
            (stub (make-temp-file "rootstack-build-stub" nil ".el"))
            (compiled (concat build "c")))
       (unwind-protect
           (progn
             (with-temp-file stub
               (insert ";;; -*- lexical-binding: t; -*-\n"
                       (format "(load %S nil t t)\n" build)
                       "(setq nelisp-standalone--root-pin-slots 8192)\n"))
             (should (byte-compile-file stub))
             (rename-file (concat stub "c") compiled t)
             (set-file-times compiled (time-add (current-time) 60))
             (should (file-newer-than-file-p compiled build))
             (should (equal "Native layout oracle disagreement: nl_root_pin_region syntactic=(4253888 . 524288) native=(4253888 . 262144)"
                            (error-message-string (should-error (wasm-rootstack-authority))))))
         (delete-file stub))))))

(ert-deftest wasm-rootstack/r8-q6-cl-incf ()
  (wasm-rootstack-full--reject-native-copy
   "(cl-incf nelisp-standalone--root-pin-slots -8192)"
   "Native layout oracle disagreement: nl_root_pin_region syntactic=(4253888 . 524288) native=(4253888 . 262144)"))

(ert-deftest wasm-rootstack/r8-native-values-and-inputs ()
  (let* ((process-environment (copy-sequence process-environment))
         (wasm-rootstack--oracle-cache (make-hash-table :test #'equal)))
    ;; Host input must not enable the Windows branch, reload or callsites.
    (setenv "NELISP_RUNTIME_RELOAD" "1")
    (setenv "NELISP_STANDALONE_TARGET" "windows-x86_64")
    (let* ((before (symbol-function 'nelisp-standalone--root-pin-region-offset))
           (authority (wasm-rootstack-authority))
           (oracle (plist-get authority :native-oracle))
           (files (plist-get oracle :files)))
      (should (eq before (symbol-function 'nelisp-standalone--root-pin-region-offset)))
      (should (equal (plist-get oracle :values)
                     '(("nl_rootstack_top" . 8) ("nl_rootstack_region" . 16)
                       ("nl_gc_diag" . 4194320) ("nl_thread_parallel_ctx" . 4252192)
                       ("nl_alloc_diag" . 4252256) ("nl_thread_registry" . 4252352)
                       ("nl_gc_stats" . 4253392) ("nl_root_pin_control" . 4253824)
                       ("nl_root_pin_region" . 4253888) ("nl_eln_callback_context" . 4778176))))
      (should (> (length files) 20))
      (should (assoc (plist-get authority :file) files))
      (should (assoc wasm-rootstack--oracle-helper files))
      (should (wasm-rootstack--oracle-files-current-p files))
      (should (equal (getenv "NELISP_RUNTIME_RELOAD") "1")))))

(ert-deftest wasm-rootstack/r8-native-cache-load-closure ()
  (let ((wasm-rootstack--oracle-cache (make-hash-table :test #'equal)))
    (wasm-rootstack-full--with-native-copy
     "(require 'audit-redef)\n(with-temp-buffer (insert \"run\\n\") (write-region (point-min) (point-max) (expand-file-name \"runs\" nelisp-standalone--repo-root) t 'silent))"
     (lambda (root)
       (let* ((file (expand-file-name "scripts/nelisp-standalone-build.el" root))
              (extra (expand-file-name "lisp/audit-redef.el" root))
              (oracle (plist-get (wasm-rootstack-authority) :native-oracle)))
         (should (equal (cdr (assoc extra (plist-get oracle :files))) (wasm-rootstack--hash extra)))
         (should (eq oracle (plist-get (wasm-rootstack-authority) :native-oracle)))
         (with-temp-buffer
           (insert-file-contents (expand-file-name "runs" root))
           (should (equal (buffer-string) "run\n")))
         ;; Same build path and bytes, changed required file: must launch anew.
         (with-temp-file extra (insert "(defconst audit-value 2)\n(provide 'audit-redef)\n"))
         (should-not (equal oracle (plist-get (wasm-rootstack-authority) :native-oracle)))
         (with-temp-buffer
           (insert-file-contents (expand-file-name "runs" root))
           (should (equal (buffer-string) "run\nrun\n")))
         ;; A fresh semantic change in that file must not reuse stale values.
         (with-temp-file extra
           (insert "(set-default 'nelisp-standalone--root-pin-slots 8192)\n(provide 'audit-redef)\n"))
         (should (equal
                  "Native layout oracle disagreement: nl_root_pin_region syntactic=(4253888 . 524288) native=(4253888 . 262144)"
                  (error-message-string (should-error (wasm-rootstack-authority)))))
         ;; Change only build bytes; the path alone cannot key the cache.
         (with-temp-file extra (insert "(defconst audit-value 1)\n(provide 'audit-redef)\n"))
         (should (eq oracle (plist-get (wasm-rootstack-authority) :native-oracle)))
         (with-temp-buffer
           (insert "\n;; Changed build bytes.\n")
           (write-region (point-min) (point-max) file t 'silent))
         (wasm-rootstack-authority)
         (with-temp-buffer
           (insert-file-contents (expand-file-name "runs" root))
           (should (equal (buffer-string) "run\nrun\nrun\nrun\n")))
         (should (= (hash-table-count wasm-rootstack--oracle-cache) 4))))
     "(defconst audit-value 1)\n(provide 'audit-redef)\n")))

(ert-deftest wasm-rootstack/r8-native-process-failure ()
  (dolist (suffix '("(" ")" "(error \"Deliberate native load failure\")"
                    "(kill-emacs 17)" "(kill-emacs 0)"
                    "(princ \"WASM-ROOTSTACK-NATIVE=oops\\n\") (kill-emacs 0)"
                    "(error \"%s\" (make-string 20000 ?x))"))
    (wasm-rootstack-full--with-native-copy
     suffix
     (lambda (_)
       (let ((message (error-message-string (should-error (wasm-rootstack-authority)))))
         (should (string-prefix-p "Native layout oracle failed: " message))
         (should (<= (length message) 530)))))))

(ert-deftest wasm-rootstack/r8-native-process-timeout ()
  ;; Exercise the real 120s child timeout, not a mocked process or reader.
  (wasm-rootstack-full--with-native-copy
   "(sleep-for 130)"
   (lambda (_)
     (let* ((start (float-time))
            (message (error-message-string (should-error (wasm-rootstack-authority)))))
       (should (string-prefix-p "Native layout oracle failed: exit=124" message))
       (should (>= (- (float-time) start) 120))))))

(provide 'wasm-rootstack-full-test)
