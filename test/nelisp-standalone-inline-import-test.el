;;; nelisp-standalone-inline-import-test.el --- Unit import inlining -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'ert)
(require 'cl-lib)
(defvar nelisp-standalone--inline-counter)
(declare-function nelisp-standalone--cached-unit "nelisp-standalone-build"
                  (name source &rest ignored))
(declare-function nelisp-standalone--inline-source "nelisp-standalone-build"
                  (source imports))
(declare-function nelisp-standalone--inline-constant-base-case "nelisp-standalone-build"
                  (definition args))
(let ((load-path (cons (expand-file-name "../lisp"
                                        (file-name-directory (or load-file-name buffer-file-name)))
                       load-path)))
  (require 'nelisp-heap-accessors))

(defconst nelisp-inline-import-test--source
  (or (getenv "NELISP_INLINE_TEST_SOURCE")
      (expand-file-name "../scripts/nelisp-standalone-build.el"
                        (file-name-directory (or load-file-name buffer-file-name)))))

;; Exercise the real orchestration functions without starting a native build.
(with-temp-buffer
  (insert-file-contents nelisp-inline-import-test--source)
  (dolist (name '(nelisp-standalone--inline-pure-ops
                  nelisp-standalone--inline-boundaries
                  nelisp-standalone--inline-max-nodes
                  nelisp-standalone--inline-counter
                  nelisp-standalone--inline-stats
                  nelisp-standalone--inline-import-sources
                  nelisp-standalone--unit-inline-dependencies
                  nelisp-standalone--inline-static-data-p
                  nelisp-standalone--prepare-inline-import
                  nelisp-standalone--inline-body-ok-p
                  nelisp-standalone--inline-rename
                  nelisp-standalone--inline-level
                  nelisp-standalone--inline-clone-p
                  nelisp-standalone--inline-constant-base-case
                  nelisp-standalone--inline-tree-symbols
                  nelisp-standalone--inline-assigned-symbols
                  nelisp-standalone--inline-flat-plan
                  nelisp-standalone--inline-small-defuns-imports
                  nelisp-standalone--inline-source
                  nelisp-standalone--build-trace-phase
                  nelisp-standalone--unit-cache-key
                  nelisp-standalone--cached-unit
                  nelisp-standalone--glue-source
                  nelisp-standalone--unit-inline-imports
                  nelisp-standalone--direct-only-unit-defuns
                  nelisp-standalone--direct-only-for
                  nelisp-standalone--xinline-table
                  nelisp-standalone--xinline-used
                  nelisp-standalone--runtime-reload-enabled-p
                  nelisp-standalone--xinline-enabled-p
                  nelisp-standalone--xinline-digest
                  nelisp-standalone--unit-for
                  nelisp-standalone--reader-driver-unit))
    (goto-char (point-min))
    (when (re-search-forward
           (format "^(\\(?:defun\\|defvar\\|defconst\\|defmacro\\) %s\\_>"
                   (regexp-quote (symbol-name name))) nil t)
      (goto-char (match-beginning 0))
      (eval (read (current-buffer)) t))))

(defvar nelisp-standalone--inline-import-sources nil)
(defvar nelisp-standalone--manifest nil)
(defvar nelisp-standalone--gc-source nil)
(defvar nelisp-standalone--strings-source nil)
;; Compiler policy variables the unit cache key records (the compiler is not loaded here).
(defvar nelisp-aot-compiler--x86-jump-tables-enabled nil)
(defvar nelisp-aot-compiler--x86-direct-only-defuns nil)
(defvar nelisp-aot-compiler--x86-call-free-regions nil)
(defvar nelisp-aot-compiler--x86-affine-addresses nil)
(defvar nelisp-standalone--this-file nil)
(defvar nelisp-standalone--build-trace-path nil)
(defvar nelisp-standalone--build-trace-unit nil)
(defvar nelisp-standalone--recompiled nil)
(defvar nelisp-inline-import-test--feature-source nil)
(defconst nelisp-inline-import-test--donor
  '(seq (defun donor (a b) (+ (* a 10) b))))
;; The strings line's donor, which `nelisp-standalone--unit-inline-dependencies' adds after the allocator.
(defconst nelisp-inline-import-test--strings-donor
  '(seq (defun strings_donor (a) (+ a 5))))

(defun nelisp-inline-import-test--host (form)
  (cond ((atom form) form)
        ((eq (car form) 'quote) form)
        (t (cons (if (eq (car form) 'seq) 'progn (car form))
                 (mapcar #'nelisp-inline-import-test--host (cdr form))))))

(defun nelisp-inline-import-test--function (definition)
  (eval (cons 'lambda (nelisp-inline-import-test--host (cddr definition))) t))

(ert-deftest nelisp-inline-import-bounded-four-pass-chain ()
  (cl-labels ((calls-p (name tree)
                (and (consp tree)
                     (or (eq (car tree) name)
                         (calls-p name (car tree))
                         (calls-p name (cdr tree))))))
    (let ((source '(seq (defun caller ()
                         (let* ((a__i1 99) (trace nil)
                                (answer (chain_one (progn (push 'once trace) 23))))
                           (list answer a__i1 trace)))))
          (imports '((seq (defun chain_one (a) (chain_two a))
                          (defun chain_two (a) (chain_three a))
                          (defun chain_three (a) (chain_four a))
                          (defun chain_four (a) (+ a 1))))))
      (dolist (level '("0" "1" "2" "3" "4"))
        (let* ((process-environment (cons (concat "NELISP_INLINE=" level)
                                          process-environment))
               (nelisp-standalone--inline-counter 0)
               (result (nelisp-standalone--inline-source source imports))
               (body (cdddr (cadr result))))
          (pcase level
            ("0" (should (equal result source)))
            ("1" (should (calls-p 'chain_two body))
                 (should-not (calls-p 'chain_one body)))
            ("2" (should (calls-p 'chain_three body))
                 (should-not (calls-p 'chain_two body)))
            ("3" (should (calls-p 'chain_four body))
                 (should-not (calls-p 'chain_three body)))
            ("4" (dolist (name '(chain_one chain_two chain_three chain_four))
                   (should-not (calls-p name body)))
                 (should (equal (funcall (nelisp-inline-import-test--function
                                         (cadr result)))
                                '(24 99 (once)))))))))))


(ert-deftest nelisp-inline-import-preserves-authenticated-evaluator-boundaries ()
  ;; Small evaluator trampolines are still proof boundaries. Exercise both
  ;; candidate collectors and the first binding initializer traversal.
  (cl-labels ((calls-p (name tree)
                (and (consp tree)
                     (or (eq (car tree) name)
                         (calls-p name (car tree))
                         (calls-p name (cdr tree))))))
    (let ((process-environment (cons "NELISP_INLINE=2" process-environment)))
      (dolist (boundary '(nl_apply_function nl_apply_function_w))
        (dolist (mode '(local imported))
          (let* ((definitions
                  `((defun ordinary_helper (x) (+ x 1))
                    (defun ,boundary (function args env out)
                      (raw_apply function args env out))))
                 (caller
                  `(defun caller (function args env out)
                     (let* ((result (,boundary (ordinary_helper function)
                                               args env out)))
                       result)))
                 (source (cons 'seq (append (when (eq mode 'local) definitions)
                                            (list caller))))
                 (imports (when (eq mode 'imported)
                            (list (cons 'seq definitions))))
                 (first (nelisp-standalone--inline-small-defuns-imports source imports))
                 (second (nelisp-standalone--inline-small-defuns-imports first imports))
                 (body (cdddr (cl-find 'caller (cdr second) :key #'cadr))))
            (should (calls-p boundary body))
            (should-not (calls-p 'raw_apply body))
            (should-not (calls-p 'ordinary_helper body))
            (should (calls-p '+ body))))))))

(ert-deftest nelisp-inline-import-preserves-argument-order-and-count ()
  (let* ((process-environment (cons "NELISP_INLINE=2" process-environment))
         (source '(seq (defun caller ()
                         (let* ((trace nil)
                                (result (donor (progn (push 'left trace) 2)
                                               (progn (push 'right trace) 7))))
                           (list result trace)))))
         (output (nelisp-standalone--inline-small-defuns-imports
                  source (list nelisp-inline-import-test--donor))))
    (should (= (length (cdr output)) 1)) ; Never emit donor definitions.
    (should (equal (funcall (nelisp-inline-import-test--function (cadr output)))
                   '(27 (right left))))
    (let ((process-environment (cons "NELISP_INLINE=0" process-environment)))
      (should (equal source (nelisp-standalone--inline-small-defuns-imports
                            source (list nelisp-inline-import-test--donor)))))))

(ert-deftest nelisp-inline-import-ineligible-local-still-shadows-donor ()
  (let* ((source '(seq (defun donor (a b) (progn 999))
                       (defun caller () (donor 2 7))))
         (output (nelisp-standalone--inline-small-defuns-imports
                  source (list nelisp-inline-import-test--donor))))
    (should (equal (nth 3 (nth 2 output)) '(donor 2 7)))))

(ert-deftest nelisp-inline-import-extern-call-preserves-argument-order-and-count ()
  (let* ((source '(seq (defun caller ()
                         (let* ((trace nil)
                                (result (extern-call donor
                                          (progn (push 'left trace) 2)
                                          (progn (push 'right trace) 7))))
                           (list result trace)))))
         (output (nelisp-standalone--inline-small-defuns-imports
                  source (list nelisp-inline-import-test--donor))))
    (should (= (length (cdr output)) 1))
    (should (equal (funcall (nelisp-inline-import-test--function (cadr output)))
                   '(27 (right left))))))

(ert-deftest nelisp-inline-import-extern-call-keeps-abi-and-local-boundaries ()
  (dolist (call '((extern-call donor (:i64 2) 7)
                  (extern-call donor 2 (:f64 7.0))
                  (extern-call donor 2 (:varargs 7))
                  (extern-call-f64 donor 2 7)
                  (extern-call donor 2)
                  (extern-call unknown 2 7)))
    (let ((source `(seq (defun caller () ,call))))
      (should (equal source (nelisp-standalone--inline-small-defuns-imports
                            source (list nelisp-inline-import-test--donor))))))
  (dolist (local '((defun donor (a b) (+ a b))
                   (defun donor (a b) (progn 999))))
    (let* ((source `(seq ,local (defun caller () (extern-call donor 2 7))))
           (output (nelisp-standalone--inline-small-defuns-imports
                    source (list nelisp-inline-import-test--donor))))
      (should (equal (nth 3 (nth 2 output)) '(extern-call donor 2 7))))))

(ert-deftest nelisp-inline-import-feature-unit-binds-imports-before-cache ()
  (let ((nelisp-inline-import-test--feature-source '(seq (defun caller () 1)))
        observed)
    (cl-letf (((symbol-function 'require) (lambda (&rest _) t))
              ((symbol-function 'locate-library) (lambda (_) "feature.el"))
              ((symbol-function 'nelisp-standalone--unit-inline-imports)
               (lambda (name)
                 (should (equal name "mirror-lookup.o"))
                 (list nelisp-inline-import-test--donor)))
              ((symbol-function 'nelisp-standalone--cached-unit)
               (lambda (&rest _)
                 (setq observed nelisp-standalone--inline-import-sources))))
      (nelisp-standalone--unit-for
       '("mirror-lookup.o" feature nelisp-inline-import-test--feature-source))
      (should (equal observed (list nelisp-inline-import-test--donor))))))

(ert-deftest nelisp-inline-import-reader-driver-binds-before-cache ()
  ;; This cold-loader path bypasses the manifest's `--unit-for' entry.
  (let ((nelisp-standalone--manifest '(("arena.o" :glue allocator) ("string-blocks.o" :glue strings)))
        (nelisp-standalone--this-file "fixture.el") observed)
    (cl-letf (((symbol-function 'nelisp-standalone--reader-driver-source)
               (lambda () '(seq (defun caller () (donor 2 7)))))
              ((symbol-function 'nelisp-standalone--glue-source)
               (lambda (name source)
                 (pcase name
                   ("arena.o" (should (eq source 'allocator)) nelisp-inline-import-test--donor)
                   ("string-blocks.o" (should (eq source 'strings)) nelisp-inline-import-test--strings-donor)
                   (_ (ert-fail (list "unexpected donor" name))))))
              ((symbol-function 'nelisp-standalone--prepare-inline-import) #'identity)
              ((symbol-function 'nelisp-standalone--cached-unit)
               (lambda (&rest _) (setq observed nelisp-standalone--inline-import-sources))))
      (nelisp-standalone--reader-driver-unit)
      (should (equal observed (list nelisp-inline-import-test--donor
                                    nelisp-inline-import-test--strings-donor))))))

(ert-deftest nelisp-inline-import-rejects-conflicting-definitions ()
  (should-error
   (nelisp-standalone--inline-small-defuns-imports
    '(seq (defun caller () (donor 2 7)))
    (list nelisp-inline-import-test--donor
          '(seq (defun donor (a b) (progn 999)))))
   :type 'error))

(ert-deftest nelisp-inline-import-keeps-arity-and-clone-guards ()
  (dolist (source '((seq (defun caller () (donor 2)))
                    (seq (defun caller () (seq (syscall-direct 56 0) (donor 2 7))))))
    (should (equal source (nelisp-standalone--inline-small-defuns-imports
                          source (list nelisp-inline-import-test--donor))))))

(ert-deftest nelisp-inline-import-source-data-excluded-before-target-rewrite ()
  (cl-letf (((symbol-function 'nelisp-standalone--abi-box-split-source) #'identity)
            ((symbol-function 'nelisp-standalone--rebase-arena-source) #'identity)
            ((symbol-function 'nelisp-standalone-arena-rewrite-source)
             (lambda (source) (cl-subst '(data-addr nl_arena_base) 'arena-base source))))
    (let ((output (nelisp-standalone--prepare-inline-import
                   '(seq (data-blob private "x" rodata)
                         (defun private_address () (data-addr private))
                         (defun shared_address () (+ arena-base 8))))))
      (should (equal (plist-get output :private) '(private_address)))
      (should (equal (nth 3 (nth 2 (plist-get output :source)))
                     '(+ (data-addr nl_arena_base) 8)))
      (let ((rewritten (nelisp-standalone--inline-small-defuns-imports
                        '(seq (defun caller () (+ (private_address) (shared_address))))
                        (list output))))
        (should (equal (nth 1 (nth 3 (cadr rewritten))) '(private_address))))
      (should-error
       (nelisp-standalone--inline-small-defuns-imports
        '(seq (defun caller () (private_address)))
        (list output '(seq (defun private_address () 999))))
       :type 'error))))

(ert-deftest nelisp-inline-import-cache-key-covers-donor-on-every-lookup ()
  (cl-letf (((symbol-function 'nelisp-standalone--toolchain-digest) (lambda () "fixed")))
    (let* ((source '(seq (defun caller () (donor 2 7))))
           (nelisp-standalone--inline-import-sources (list nelisp-inline-import-test--donor))
           (first (nelisp-standalone--unit-cache-key source)))
      (should (equal first (nelisp-standalone--unit-cache-key source)))
      (setq nelisp-standalone--inline-import-sources
            '((seq (defun donor (a b) (+ (* a 11) b)))))
      (should-not (equal first (nelisp-standalone--unit-cache-key source))))))

(ert-deftest nelisp-inline-import-resolves-target-selected-gc ()
  (let ((nelisp-standalone--manifest
         '(("reader-gc.o" :glue nelisp-standalone--gc-source)
           ("string-blocks.o" :glue nelisp-standalone--strings-source)))
        (nelisp-standalone--gc-source '(seq (defun raw () 1)))
        (nelisp-standalone--strings-source nelisp-inline-import-test--strings-donor))
    (cl-letf (((symbol-function 'nelisp-standalone--target-gc-source)
               (lambda () '(seq (defun selected () 2))))
              ((symbol-function 'nelisp-standalone--prepare-inline-import) #'identity))
      (should (equal (nelisp-standalone--unit-inline-imports "arena.o")
                     (list '(seq (defun selected () 2)) nelisp-inline-import-test--strings-donor))))))

(ert-deftest nelisp-inline-import-real-gc-membership-and-negative-control ()
  (let* ((gc (with-temp-buffer
               (insert-file-contents nelisp-inline-import-test--source)
               (goto-char (point-min))
               (search-forward "(defconst nelisp-standalone--gc-source")
               (goto-char (match-beginning 0))
               (cadr (nth 2 (read (current-buffer))))))
         ;; Production lowers storage accessors before import inlining.
         (definition (nelisp-heap-accessors-lower
                      (cl-find 'nl_gc_in_arena (cdr gc) :key #'cadr)))
         (source '(seq (defun caller (address) (nl_gc_in_arena address))))
         (output (nelisp-standalone--inline-small-defuns-imports source (list (list 'seq definition))))
         (reference (nelisp-inline-import-test--function definition))
         (candidate (nelisp-inline-import-test--function (cadr output)))
         (memory (make-hash-table :test 'eql))
         (cases '(0 1000 1099 1100 1399 1400 2099 2100 2399 2400 3099 3100 3399 3400)))
    (dolist (pair '((268436160 . 10000) (268436168 . 30000) (268435456 . 400)
                    (10000 . 1000) (10024 . 1100) (10048 . 20000)
                    (20000 . 2000) (20016 . 400) (20024 . 2100) (20048 . 30000)
                    (30000 . 3000) (30016 . 400) (30024 . 3100) (30048 . 0)))
      (puthash (car pair) (cdr pair) memory))
    (cl-letf (((symbol-function 'ptr-read-u64)
               (lambda (address offset)
                 (let ((value (gethash (+ address offset) memory 'missing)))
                   (when (eq value 'missing) (error "Unknown model address")) value))))
      (should (equal (mapcar candidate cases) '(0 0 0 1 1 0 0 1 1 0 0 1 1 0)))
      (should (equal (mapcar candidate cases) (mapcar reference cases)))
      ;; A broken replacement returning zero must be detected by these cases.
      (should-error (should (equal (mapcar (lambda (_) 0) cases)
                                  (mapcar reference cases))) :type 'ert-test-failed)
      (puthash 268436160 0 memory) (puthash 268436168 0 memory)
      (should (= (funcall candidate 1100) 0)))))

(ert-deftest nelisp-inline-import-reader-unit-cache-tracks-donor-content ()
  ;; Reader-only units call the real cache directly, with no manifest wrapper.
  ;; Changing a donor must miss the cache and compile with that same donor.
  (let* ((directory (make-temp-file "nelisp-reader-import-" t))
         (nelisp-standalone--inline-import-sources nil)
         (nelisp-standalone--build-trace-path nil)
         (nelisp-standalone--recompiled nil)
         (donor '(seq (defun donor () 1)))
         (compiled nil))
    (unwind-protect
        (cl-letf (((symbol-function 'nelisp-standalone--abi-box-split-source) #'identity)
                  ((symbol-function 'nelisp-standalone--target-object-name)
                   (lambda (name) (concat "target-" name)))
                  ((symbol-function 'nelisp-standalone--target-cache-dir)
                   (lambda () directory))
                  ((symbol-function 'nelisp-standalone--unit-inline-imports)
                   (lambda (name)
                     (should (equal name "reader-only.o"))
                     (list donor)))
                  ((symbol-function 'nelisp-standalone--toolchain-digest)
                   (lambda () "fixture-toolchain"))
                  ((symbol-function 'nelisp-standalone--compile-to-unit)
                   (lambda (_name _source)
                     (push (copy-tree nelisp-standalone--inline-import-sources) compiled)
                     (list 'compiled (copy-tree nelisp-standalone--inline-import-sources))))
                  ((symbol-function 'nelisp-standalone--box-census-record)
                   (lambda (&rest _) nil))
                  ((symbol-function 'nelisp-standalone--unit-cache-encode) #'identity)
                  ((symbol-function 'nelisp-standalone--unit-cache-decode) #'identity))
          (let* ((source '(seq (defun caller () (donor))))
                 (first (nelisp-standalone--cached-unit "reader-only.o" source)))
            (should (equal first (nelisp-standalone--cached-unit "reader-only.o" source)))
            (should (= (length compiled) 1))
            (setq donor '(seq (defun donor () 2)))
            (should-not (equal first (nelisp-standalone--cached-unit "reader-only.o" source)))
            (should (= (length compiled) 2))
            (should (equal (car compiled) (list donor)))))
      (delete-directory directory t))))

(ert-deftest nelisp-inline-import-single-definition-and-every-binding ()
  ;; The first initializer and a singleton unit must reach the donor too.
  ;; Count/ordering assertions use the actual expanded Lisp, not its shape.
  (dolist (binding-kind '(let let*))
    (dolist (singleton '(nil t))
      (let* ((definition
              `(defun caller (x trace)
                 (,binding-kind ((a (donor (progn (setq trace (append trace '(first))) x) 2))
                                 (b (donor (progn (setq trace (append trace '(second))) x) 3)))
                   (list (+ a b) trace))))
             (source (if singleton definition (list 'seq definition)))
             (output (nelisp-standalone--inline-small-defuns-imports
                      source (list nelisp-inline-import-test--donor)))
             (expanded (if singleton output (cadr output))))
        (should-not (string-match-p "(donor " (prin1-to-string expanded)))
        (should (equal (funcall (nelisp-inline-import-test--function expanded) 4 nil)
                       '(85 (first second))))))))

(ert-deftest nelisp-inline-import-generated-bindings-cannot-capture-caller ()
  (dolist (source '((defun caller (a__i1) (donor 2 a__i1))
                    (seq (defun caller (a__i1) (donor 2 a__i1)))
                    (seq (defun caller (a__i1)
                           (let* ((answer (donor 2 a__i1))) answer)))))
    (let* ((nelisp-standalone--inline-counter 0)
           (output (nelisp-standalone--inline-small-defuns-imports
                    source (list nelisp-inline-import-test--donor)))
           (definition (if (eq (car output) 'defun) output (cadr output))))
      (should (= (funcall (nelisp-inline-import-test--function definition) 4) 24)))))

(defun nelisp-inline-basecase-test--calls-p (name tree)
  (and (consp tree)
       (or (eq (car tree) name)
           (nelisp-inline-basecase-test--calls-p name (car tree))
           (nelisp-inline-basecase-test--calls-p name (cdr tree)))))
(defun nelisp-inline-basecase-test--body (source name)
  (cdddr (cl-find name (cdr source) :key #'cadr)))
(defun nelisp-inline-basecase-test--opt (source &optional imports)
  (let ((process-environment (cons "NELISP_INLINE=1" process-environment)))
    (nelisp-standalone--inline-small-defuns-imports source imports)))
(defun nelisp-inline-basecase-test--host (form)
  (cond ((atom form) form)
        ((eq (car form) 'quote) form)
        (t (cons (if (eq (car form) 'seq) 'progn
                   (nelisp-inline-basecase-test--host (car form)))
                 (nelisp-inline-basecase-test--host (cdr form))))))
(defun nelisp-inline-basecase-test--function (source name)
  (eval (cons 'lambda
              (nelisp-inline-basecase-test--host
               (cddr (cl-find name (cdr source) :key #'cadr)))) t))
(defconst nelisp-inline-basecase-test--recursive
  '(defun basecase_helper (a n unused)
     (if (= n 0) (+ a 1) (basecase_helper a (- n 1) unused))))

(ert-deftest nelisp-inline-basecase-literal-local-and-imported ()
  (dolist (mode '(local imported))
    (let* ((source `(seq ,@(when (eq mode 'local) (list nelisp-inline-basecase-test--recursive))
                        (defun caller (a) (basecase_helper a 0 9))))
           (result (nelisp-inline-basecase-test--opt source (when (eq mode 'imported)
                                               (list (list 'seq nelisp-inline-basecase-test--recursive))))))
      (should-not (nelisp-inline-basecase-test--calls-p 'basecase_helper (nelisp-inline-basecase-test--body result 'caller)))
      (should (= (funcall (nelisp-inline-basecase-test--function result 'caller) 7) 8)))))

(ert-deftest nelisp-inline-basecase-every-actual-once-in-order ()
  (let* ((source `(seq ,nelisp-inline-basecase-test--recursive
                      (defun caller ()
                        (let* ((trace 0)
                               (answer (basecase_helper
                                        (seq (setq trace (+ (* trace 10) 1)) 7)
                                        0
                                        (seq (setq trace (+ (* trace 10) 2)) 99))))
                          (+ (* answer 100) trace)))))
         (result (nelisp-inline-basecase-test--opt source)))
    (should-not (nelisp-inline-basecase-test--calls-p 'basecase_helper (nelisp-inline-basecase-test--body result 'caller)))
    (should (= (funcall (nelisp-inline-basecase-test--function result 'caller)) 812))))

(ert-deftest nelisp-inline-basecase-hygiene-and-branch-shadowing ()
  (let* ((source '(seq (defun shadow_helper (a n)
                        (if (= n 0) (let* ((a (+ a 3)) (n 8)) (+ a n))
                          (shadow_helper a (- n 1))))
                      (defun caller (a)
                        (let* ((a__i1 100) (n__i2 200))
                          (+ (shadow_helper a 0) a__i1 n__i2)))))
         (nelisp-standalone--inline-counter 0)
         (result (nelisp-inline-basecase-test--opt source)))
    (should-not (nelisp-inline-basecase-test--calls-p 'shadow_helper (nelisp-inline-basecase-test--body result 'caller)))
    (should (= (funcall (nelisp-inline-basecase-test--function result 'caller) 7) 318))))

(ert-deftest nelisp-inline-basecase-does-not-fold-under-mutation ()
  (let* ((source '(seq (defun mutate_helper (a n)
                        (if (= n 0)
                            (let* ((n 1)) (seq (setq n 2) (if (= n 0) 77 a)))
                          (mutate_helper a (- n 1))))
                      (defun caller () (mutate_helper 99 0))))
         (result (nelisp-inline-basecase-test--opt source)))
    (should-not (nelisp-inline-basecase-test--calls-p 'mutate_helper (nelisp-inline-basecase-test--body result 'caller)))
    (should (= (funcall (nelisp-inline-basecase-test--function result 'caller)) 99))))

(ert-deftest nelisp-inline-basecase-all-simple-comparisons ()
  (dolist (spec '((= 0 0) (/= 1 0) (< -1 0) (> 1 0) (<= 0 0) (>= 0 0)))
    (pcase-let* ((`(,op ,literal ,limit) spec)
                 (source `(seq (defun cmp_helper (a n)
                                 (if (,op n ,limit) (+ a 1) (cmp_helper a n)))
                               (defun caller () (cmp_helper 7 ,literal))))
                 (result (nelisp-inline-basecase-test--opt source)))
      (should-not (nelisp-inline-basecase-test--calls-p 'cmp_helper (nelisp-inline-basecase-test--body result 'caller)))
      (should (= (funcall (nelisp-inline-basecase-test--function result 'caller)) 8)))))

(ert-deftest nelisp-inline-basecase-selects-false-branch ()
  (let* ((source '(seq (defun false_helper (a n)
                        (if (= n 0) (false_helper a n) (+ a 1)))
                      (defun caller () (false_helper 7 1))))
         (result (nelisp-inline-basecase-test--opt source)))
    (should-not (nelisp-inline-basecase-test--calls-p 'false_helper (nelisp-inline-basecase-test--body result 'caller)))
    (should (= (funcall (nelisp-inline-basecase-test--function result 'caller)) 8))))

(ert-deftest nelisp-inline-basecase-known-two-formals ()
  (let* ((source '(seq (defun two_helper (a left right)
                        (if (= left right) (+ a 1) (two_helper a left right)))
                      (defun caller () (two_helper 7 3 3))))
         (result (nelisp-inline-basecase-test--opt source)))
    (should-not (nelisp-inline-basecase-test--calls-p 'two_helper (nelisp-inline-basecase-test--body result 'caller)))
    (should (= (funcall (nelisp-inline-basecase-test--function result 'caller)) 8))))

(ert-deftest nelisp-inline-basecase-unknown-or-nonliteral-guard-remains ()
  (dolist (call '((basecase_helper a n 9) (basecase_helper a (+ 0 0) 9)
                  (basecase_helper a (side_effect) 9) (basecase_helper a 1 9)))
    (let* ((source `(seq ,nelisp-inline-basecase-test--recursive (defun caller (a n) ,call)))
           (result (nelisp-inline-basecase-test--opt source)))
      (should (nelisp-inline-basecase-test--calls-p 'basecase_helper (nelisp-inline-basecase-test--body result 'caller))))))

(ert-deftest nelisp-inline-basecase-no-guard-fold-under-let ()
  (let* ((source '(seq (defun let_helper (a n)
                        (let* ((n 0)) (if (= n 0) a (let_helper a n))))
                      (defun caller (a) (let_helper a 0))))
         (result (nelisp-inline-basecase-test--opt source)))
    (should (nelisp-inline-basecase-test--calls-p 'let_helper (nelisp-inline-basecase-test--body result 'caller)))))

(ert-deftest nelisp-inline-basecase-unsafe-guard-remains ()
  (dolist (guard '((= (side_effect) n) (side_effect n 0) (and (= n 0) 1)))
    (let* ((source `(seq (defun guard_helper (a n)
                          (if ,guard a (guard_helper a n)))
                        (defun caller (a) (guard_helper a 0))))
           (result (nelisp-inline-basecase-test--opt source)))
      (should (nelisp-inline-basecase-test--calls-p 'guard_helper (nelisp-inline-basecase-test--body result 'caller))))))

(ert-deftest nelisp-inline-basecase-recursive-selected-branch-remains ()
  (let* ((source `(seq ,nelisp-inline-basecase-test--recursive (defun caller (a) (basecase_helper a 2 9))))
         (result (nelisp-inline-basecase-test--opt source)))
    (should (nelisp-inline-basecase-test--calls-p 'basecase_helper (nelisp-inline-basecase-test--body result 'caller)))))

(ert-deftest nelisp-inline-basecase-unsafe-and-clone-bodies-remain ()
  (dolist (branch '((alloc-bytes 32 8) (syscall-direct 56 0 0 0 0 0)
                   (quote a) (funcall target a)))
    (let* ((source `(seq (defun unsafe_helper (a n)
                          (if (= n 0) ,branch (unsafe_helper a n)))
                        (defun caller (a) (unsafe_helper a 0))))
           (result (nelisp-inline-basecase-test--opt source)))
      (should (nelisp-inline-basecase-test--calls-p 'unsafe_helper (nelisp-inline-basecase-test--body result 'caller))))))

(ert-deftest nelisp-inline-basecase-retains-whole-body-size-cap ()
  (let* ((large (cons '+ (make-list 90 '(+ 1 2))))
         (source `(seq (defun large_helper (a n)
                         (if (= n 0) a (seq ,large (large_helper a n))))
                       (defun caller (a) (large_helper a 0))))
         (result (nelisp-inline-basecase-test--opt source)))
    (should (nelisp-inline-basecase-test--calls-p 'large_helper (nelisp-inline-basecase-test--body result 'caller)))))

(ert-deftest nelisp-inline-basecase-i64-only ()
  (dolist (value (list (expt 2 63) (- -1 (expt 2 63))))
    (let* ((source `(seq ,nelisp-inline-basecase-test--recursive
                        (defun caller (a) (basecase_helper a ,value 9))))
           (result (nelisp-inline-basecase-test--opt source)))
      (should (nelisp-inline-basecase-test--calls-p 'basecase_helper (nelisp-inline-basecase-test--body result 'caller))))))

(ert-deftest nelisp-inline-basecase-local-shadows-import ()
  (let* ((source '(seq (defun local_helper (a n)
                        (if (= n a) (+ a 2) (local_helper a n)))
                      (defun caller (a) (local_helper a 0))))
         (imports '((seq (defun local_helper (a n)
                           (if (= n 0) (+ a 1) (local_helper a n))))))
         (result (nelisp-inline-basecase-test--opt source imports)))
    (should (nelisp-inline-basecase-test--calls-p 'local_helper (nelisp-inline-basecase-test--body result 'caller)))))

(ert-deftest nelisp-inline-basecase-private-import-excluded ()
  (let* ((source '(seq (defun caller (a) (private_helper a 0))))
         (imports '((:source (seq (defun private_helper (a n)
                                   (if (= n 0) (data-addr private_bytes)
                                     (private_helper a n))))
                            :private (private_helper))))
         (result (nelisp-inline-basecase-test--opt source imports)))
    (should (nelisp-inline-basecase-test--calls-p 'private_helper (nelisp-inline-basecase-test--body result 'caller)))))

(ert-deftest nelisp-inline-basecase-import-conflicts-remain-errors ()
  (let ((source '(seq (defun caller (a) (conflict_helper a 0))))
        (imports '((seq (defun conflict_helper (a n)
                          (if (= n 0) (+ a 1) (conflict_helper a n))))
                   (seq (defun conflict_helper (a n)
                          (if (= n 0) (+ a 2) (conflict_helper a n)))))))
    (should-error (nelisp-inline-basecase-test--opt source imports))))

(ert-deftest nelisp-inline-basecase-evaluator-boundaries-stay-calls ()
  (dolist (name '(nl_apply_function nl_apply_function_w))
    (dolist (mode '(local imported))
      (let* ((definition `(defun ,name (a n) (if (= n 0) a (,name a n))))
             (source `(seq ,@(when (eq mode 'local) (list definition))
                           (defun caller (a) (,name a 0))))
             (imports (when (eq mode 'imported) (list (list 'seq definition))))
             (result (nelisp-inline-basecase-test--opt source imports)))
        (should (nelisp-inline-basecase-test--calls-p name (nelisp-inline-basecase-test--body result 'caller)))))))

(ert-deftest nelisp-inline-basecase-extern-and-typed-abi-remain ()
  (dolist (call '((extern-call foreign_helper a 0)
                  (extern-call foreign_helper (:ptr a) 0)))
    (let* ((source `(seq (defun caller (a) ,call)))
           (imports '((seq (defun foreign_helper (a n)
                             (if (= n 0) a (foreign_helper a n))))))
           (result (nelisp-inline-basecase-test--opt source imports)))
      (should (nelisp-inline-basecase-test--calls-p 'extern-call (nelisp-inline-basecase-test--body result 'caller))))))

(ert-deftest nelisp-inline-basecase-disabled-unchanged ()
  (let* ((process-environment (cons "NELISP_INLINE=0" process-environment))
         (source `(seq ,nelisp-inline-basecase-test--recursive (defun caller (a) (basecase_helper a 0 9)))))
    (should (equal source (nelisp-standalone--inline-small-defuns-imports source)))))

(ert-deftest nelisp-inline-basecase-c-varargs-local-imported-layouts ()
  ;; The marker consumes no ABI argument. No raw formal index may include it.
  (dolist (params '((&c-varargs a n) (a &c-varargs n) (a n &c-varargs)))
    (dolist (recursive '(nil t))
      (dolist (mode '(local imported))
        (let* ((definition `(defun cv_helper ,params
                              ,(if recursive '(if (= n 0) a (cv_helper (+ a 1) 0)) '(+ a n))))
               (source `(seq ,@(when (eq mode 'local) (list definition))
                             (defun caller () (cv_helper 77 1 0))))
               (imports (when (eq mode 'imported) (list (list 'seq definition))))
               (result (nelisp-inline-basecase-test--opt source imports)))
          (should (nelisp-inline-basecase-test--calls-p 'cv_helper
                    (nelisp-inline-basecase-test--body result 'caller))))))))

(ert-deftest nelisp-inline-basecase-c-varargs-extern-layouts ()
  (dolist (params '((&c-varargs a n) (a &c-varargs n) (a n &c-varargs)))
    (dolist (recursive '(nil t))
      (let* ((definition `(defun cv_foreign ,params
                            ,(if recursive '(if (= n 0) a (cv_foreign (+ a 1) 0)) '(+ a n))))
             (source '(seq (defun caller () (extern-call cv_foreign 77 1 0))))
             (result (nelisp-inline-basecase-test--opt source (list (list 'seq definition)))))
        (should (equal (nelisp-inline-basecase-test--body result 'caller)
                       '((extern-call cv_foreign 77 1 0))))))))

(ert-deftest nelisp-inline-basecase-c-varargs-direct-specializer-rejects ()
  (dolist (params '((&c-varargs a n) (a &c-varargs n) (a n &c-varargs)))
    (should-not
     (nelisp-standalone--inline-constant-base-case
      `(defun cv_helper ,params (if (= n 0) a (cv_helper (+ a 1) 0))) '(77 1 0)))))

(ert-deftest nelisp-inline-basecase-retains-all-abi-marker-exclusions ()
  (dolist (marker '(&optional &rest &c-varargs))
    (dolist (mode '(local imported))
      (let* ((definition `(defun abi_helper (a ,marker n) (+ a n)))
             (source `(seq ,@(when (eq mode 'local) (list definition))
                           (defun caller () (abi_helper 77 1 0))))
             (imports (when (eq mode 'imported) (list (list 'seq definition))))
             (result (nelisp-inline-basecase-test--opt source imports)))
        (should (nelisp-inline-basecase-test--calls-p 'abi_helper
                  (nelisp-inline-basecase-test--body result 'caller)))))))

;; Copy propagation: an atomic argument replaces its parameter instead of being bound by a `let*'
;; (a function inlined at several levels of a chain otherwise nests one `let*' per argument and per pass).
(defun nelisp-inline-import-test--caller-def (output)
  "The defun `caller' in OUTPUT, a (seq (defun ...) ...)."
  (cl-find 'caller (cdr output) :key #'cadr))

(ert-deftest nelisp-inline-flatten-substitutes-literals-and-variables ()
  (let* ((source '(seq (defun add (a b) (+ a b))
                       (defun caller (v) (add v 3))))
         (output (nelisp-standalone--inline-small-defuns-imports source nil)))
    ;; both arguments replace their parameters: no binding is left
    (should (equal (nth 3 (nelisp-inline-import-test--caller-def output)) '(+ v 3)))
    (should (= (funcall (nelisp-inline-import-test--function (nelisp-inline-import-test--caller-def output)) 4) 7))))

(ert-deftest nelisp-inline-flatten-keeps-the-binding-of-an-assigned-parameter ()
  (let* ((source '(seq (defun bump (x) (seq (setq x (+ x 1)) x))
                       (defun caller (v) (seq (bump v) v))))
         (output (nelisp-standalone--inline-small-defuns-imports source nil))
         (body (nth 3 (nelisp-inline-import-test--caller-def output))))
    ;; the callee writes its parameter: the caller's variable must not be the target
    (should (eq (car (nth 1 body)) 'let*))
    (should (= (funcall (nelisp-inline-import-test--function (nelisp-inline-import-test--caller-def output)) 41) 41))))

(ert-deftest nelisp-inline-flatten-never-substitutes-a-special-variable ()
  (let* ((source '(seq (defvar sv 0)
                       (defun add (a b) (+ a b))
                       (defun caller () (add sv 1))))
         (output (nelisp-standalone--inline-small-defuns-imports source nil))
         (def (cl-find 'caller (cdr output) :key #'cadr))
         (body (nth 3 def)))
    ;; SV is dynamic: its binding keeps the value the call saw, the literal still replaces B
    (should (eq (car body) 'let*))
    (should (eq (cadr (car (nth 1 body))) 'sv))
    (should (equal (nth 2 body) (list '+ (car (car (nth 1 body))) 1)))))

(ert-deftest nelisp-inline-flatten-keeps-a-variable-a-later-argument-assigns ()
  (let* ((source '(seq (defun mix (a b) (+ b (* a 10)))
                       (defun caller (v) (mix v (seq (setq v 5) 1)))))
         (output (nelisp-standalone--inline-small-defuns-imports source nil))
         (fn (nelisp-inline-import-test--function (nelisp-inline-import-test--caller-def output))))
    ;; the binding read V before the second argument ran
    (should (= (funcall fn 7) 71))))

(ert-deftest nelisp-inline-flatten-keeps-a-variable-the-body-binds ()
  (let* ((source '(seq (defun f (x) (let* ((v 100)) (+ x v)))
                       (defun caller (v) (f v))))
         (output (nelisp-standalone--inline-small-defuns-imports source nil))
         (fn (nelisp-inline-import-test--function (nelisp-inline-import-test--caller-def output))))
    ;; substituting V for X would let the body's own V capture it (200 instead of 101)
    (should (= (funcall fn 1) 101))))

(ert-deftest nelisp-inline-flatten-keeps-non-atomic-arguments-bound-once-in-order ()
  (let* ((source '(seq (defun mix (a b) (+ (* a 10) b))
                       (defun caller (v)
                         (let* ((trace 0))
                           ;; progn: the host evaluator's stand-in for the argument expressions
                           (mix (progn (setq trace (+ (* trace 10) 1)) v)
                                (progn (setq trace (+ (* trace 10) 2)) trace))))))
         (output (nelisp-standalone--inline-small-defuns-imports source nil))
         (fn (nelisp-inline-import-test--function (nelisp-inline-import-test--caller-def output))))
    ;; first argument 5 (trace 1), second argument trace after the second step = 12: 5 * 10 + 12
    (should (= (funcall fn 5) 62))))
