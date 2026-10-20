;;; nelisp-standalone-inline-import-test.el --- Unit import inlining -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'ert)
(require 'cl-lib)
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
                  nelisp-standalone--inline-small-defuns
                  nelisp-standalone--unit-cache-key
                  nelisp-standalone--glue-source
                  nelisp-standalone--unit-inline-imports
                  nelisp-standalone--unit-for
                  nelisp-standalone--reader-driver-unit))
    (goto-char (point-min))
    (when (re-search-forward
           (format "^(\\(?:defun\\|defvar\\|defconst\\) %s\\_>"
                   (regexp-quote (symbol-name name))) nil t)
      (goto-char (match-beginning 0))
      (eval (read (current-buffer)) t))))

(defvar nelisp-standalone--inline-import-sources nil)
(defvar nelisp-standalone--manifest nil)
(defvar nelisp-standalone--gc-source nil)
(defvar nelisp-standalone--this-file nil)
(defvar nelisp-inline-import-test--feature-source nil)
(defconst nelisp-inline-import-test--donor
  '(seq (defun donor (a b) (+ (* a 10) b))))

(defun nelisp-inline-import-test--host (form)
  (cond ((atom form) form)
        ((eq (car form) 'quote) form)
        (t (cons (if (eq (car form) 'seq) 'progn (car form))
                 (mapcar #'nelisp-inline-import-test--host (cdr form))))))

(defun nelisp-inline-import-test--function (definition)
  (eval (cons 'lambda (nelisp-inline-import-test--host (cddr definition))) t))

(ert-deftest nelisp-inline-import-preserves-argument-order-and-count ()
  (let* ((process-environment (cons "NELISP_INLINE=2" process-environment))
         (source '(seq (defun caller ()
                         (let* ((trace nil)
                                (result (donor (progn (push 'left trace) 2)
                                               (progn (push 'right trace) 7))))
                           (list result trace)))))
         (output (nelisp-standalone--inline-small-defuns
                  source (list nelisp-inline-import-test--donor))))
    (should (= (length (cdr output)) 1)) ; Never emit donor definitions.
    (should (equal (funcall (nelisp-inline-import-test--function (cadr output)))
                   '(27 (right left))))
    (let ((process-environment (cons "NELISP_INLINE=0" process-environment)))
      (should (equal source (nelisp-standalone--inline-small-defuns
                            source (list nelisp-inline-import-test--donor)))))))

(ert-deftest nelisp-inline-import-ineligible-local-still-shadows-donor ()
  (let* ((source '(seq (defun donor (a b) (progn 999))
                       (defun caller () (donor 2 7))))
         (output (nelisp-standalone--inline-small-defuns
                  source (list nelisp-inline-import-test--donor))))
    (should (equal (nth 3 (nth 2 output)) '(donor 2 7)))))

(ert-deftest nelisp-inline-import-extern-call-preserves-argument-order-and-count ()
  (let* ((source '(seq (defun caller ()
                         (let* ((trace nil)
                                (result (extern-call donor
                                          (progn (push 'left trace) 2)
                                          (progn (push 'right trace) 7))))
                           (list result trace)))))
         (output (nelisp-standalone--inline-small-defuns
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
      (should (equal source (nelisp-standalone--inline-small-defuns
                            source (list nelisp-inline-import-test--donor))))))
  (dolist (local '((defun donor (a b) (+ a b))
                   (defun donor (a b) (progn 999))))
    (let* ((source `(seq ,local (defun caller () (extern-call donor 2 7))))
           (output (nelisp-standalone--inline-small-defuns
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
  (let ((nelisp-standalone--manifest '(("arena.o" :glue allocator)))
        (nelisp-standalone--this-file "fixture.el") observed)
    (cl-letf (((symbol-function 'nelisp-standalone--reader-driver-source)
               (lambda () '(seq (defun caller () (donor 2 7)))))
              ((symbol-function 'nelisp-standalone--glue-source)
               (lambda (name source)
                 (should (equal name "arena.o")) (should (eq source 'allocator))
                 nelisp-inline-import-test--donor))
              ((symbol-function 'nelisp-standalone--prepare-inline-import) #'identity)
              ((symbol-function 'nelisp-standalone--cached-unit)
               (lambda (&rest _) (setq observed nelisp-standalone--inline-import-sources))))
      (nelisp-standalone--reader-driver-unit)
      (should (equal observed (list nelisp-inline-import-test--donor))))))

(ert-deftest nelisp-inline-import-rejects-conflicting-definitions ()
  (should-error
   (nelisp-standalone--inline-small-defuns
    '(seq (defun caller () (donor 2 7)))
    (list nelisp-inline-import-test--donor
          '(seq (defun donor (a b) (progn 999)))))
   :type 'error))

(ert-deftest nelisp-inline-import-keeps-arity-and-clone-guards ()
  (dolist (source '((seq (defun caller () (donor 2)))
                    (seq (defun caller () (seq (syscall-direct 56 0) (donor 2 7))))))
    (should (equal source (nelisp-standalone--inline-small-defuns
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
      (let ((rewritten (nelisp-standalone--inline-small-defuns
                        '(seq (defun caller () (+ (private_address) (shared_address))))
                        (list output))))
        (should (equal (nth 1 (nth 3 (cadr rewritten))) '(private_address))))
      (should-error
       (nelisp-standalone--inline-small-defuns
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
         '(("reader-gc.o" :glue nelisp-standalone--gc-source)))
        (nelisp-standalone--gc-source '(seq (defun raw () 1))))
    (cl-letf (((symbol-function 'nelisp-standalone--target-gc-source)
               (lambda () '(seq (defun selected () 2))))
              ((symbol-function 'nelisp-standalone--prepare-inline-import) #'identity))
      (should (equal (nelisp-standalone--unit-inline-imports "arena.o")
                     '((seq (defun selected () 2))))))))

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
         (output (nelisp-standalone--inline-small-defuns source (list (list 'seq definition))))
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
