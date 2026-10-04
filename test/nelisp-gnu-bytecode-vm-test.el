;;; nelisp-gnu-bytecode-vm-test.el --- bounded GNU ELC VM bridge -*- lexical-binding: t; -*-

(require 'ert)
(require 'bytecomp)
(defconst nelisp-gnu-bytecode-vm-test--host-byte-compile-file
  (symbol-function 'byte-compile-file))
(require 'nelisp-gnu-bytecode-vm)

(defconst nelisp-gnu-bytecode-vm-test--fixture
  (expand-file-name "fixtures/gnu-bytecode-vm/shared-store.el"
                    (file-name-directory (or load-file-name buffer-file-name))))

(ert-deftest nelisp-gnu-bytecode-vm/elc-docstring-byte-counts-utf8-bytes ()
  "GNU #@ counts UTF-8 bytes, including its separator, not characters."
  (should (= (nelisp-gnu-bytecode-vm--skip-trivia
              (concat "#@4 ab" (string 31) "(provide 'x)") 0)
             7))
  (should (= (nelisp-gnu-bytecode-vm--skip-trivia
              (concat "#@11 日本語" (string 31)
                      "\n(provide 'utf8)") 0)
             10))
  (should (equal
           (nelisp-gnu-bytecode-vm--read-all
            (concat "#@0" (string 31) "42"))
           '(42)))
  (should (equal
           (nelisp-gnu-bytecode-vm--read-all
            (concat "#@1\n" (string 31) "42"))
           '(42))))

(ert-deftest nelisp-gnu-bytecode-vm/elc-docstring-lookalikes-stay-data ()
  "Only top-level #@ markers are skipped, not comment or string content."
  (should (equal
           (nelisp-gnu-bytecode-vm--read-all
            "; #@999 comment\n(message \"#@999 string\")\n(provide 'marker-ok)")
           '((message "#@999 string") (provide 'marker-ok))))
  (should (equal
           (nelisp-gnu-bytecode-vm--read-all
            (concat "#@4 ab" (string 31) "(provide 'after-doc)"))
           '((provide 'after-doc))))
  ;; A docstring payload is not Lisp syntax; this unmatched close is data.
  (should (equal
           (nelisp-gnu-bytecode-vm--read-all
            (concat "#@4 )x" (string 31)
                    "(provide 'after-unbalanced-doc)"))
           '((provide 'after-unbalanced-doc)))))

(ert-deftest nelisp-gnu-bytecode-vm/elc-docstring-malformed-markers-refuse ()
  "Malformed, truncated, and UTF-8-splitting markers fail closed."
  (dolist (text (list "#@x payload" "#@0 payload" "#@99 short"
                      "#@2 日本語" "#@1X42"))
    (should-error (nelisp-gnu-bytecode-vm--skip-trivia text 0)
                  :type 'nelisp-gnu-bytecode-vm-error)))

(defun nelisp-gnu-bytecode-vm-test--elc-function (path name)
  "Read function NAME as data from GNU ELC at PATH, without evaluating forms."
  (let* ((forms (nelisp-gnu-bytecode-vm--read-all
                 (nelisp-core-read-file-as-string path)))
         (definition
          (cl-find-if
           (lambda (form)
             (and (consp form) (eq (car form) 'defalias)
                  (equal (cadr form) (list 'quote name))))
           forms)))
    (and definition (nth 2 definition))))

(ert-deftest nelisp-gnu-bytecode-vm/exact-compact-stack-ref-call1-shape ()
  "The supported one-argument GNU form lowers to BCL STACK-REF offset one."
  (let* ((function
          (byte-compile '(lambda (value)
                           (nelisp-gnu-bytecode-vm-target value))))
         (signatures (make-hash-table :test #'eq))
         lowered)
    (puthash 'nelisp-gnu-bytecode-vm-target 1 signatures)
    (setq lowered (nelisp-gnu-bytecode-vm--lower-function
                   function signatures (make-hash-table :test #'eq)))
    (should (equal (append (aref function 1) nil) '(192 1 33 135)))
    (should (equal (append (nelisp-bc-code lowered) nil)
                   '(1 0 2 1 30 1 0)))
    (should (= (nelisp-bc-stack-depth lowered) 3))))

(ert-deftest nelisp-gnu-bytecode-vm/exact-compact-stack-ref-call2-shape ()
  "GNU CALL2's two compact STACK-REFs lower to BCL CALL2."
  (let* ((function
          (byte-compile '(lambda (left right)
                           (nelisp-gnu-bytecode-vm-target2 left right))))
         (signatures (make-hash-table :test #'eq))
         lowered)
    (puthash 'nelisp-gnu-bytecode-vm-target2 2 signatures)
    (should (= (aref function 0) 514))
    (should (equal (append (aref function 1) nil) '(192 2 2 34 135)))
    (should (equal (append (aref function 2) nil)
                   '(nelisp-gnu-bytecode-vm-target2)))
    (setq lowered (nelisp-gnu-bytecode-vm--lower-function
                   function signatures (make-hash-table :test #'eq)))
    (should (equal (append (nelisp-bc-code lowered) nil)
                   '(1 0 2 2 2 2 30 2 0)))
    (should (= (nelisp-bc-stack-depth lowered) 5))))

(ert-deftest nelisp-gnu-bytecode-vm/exact-compact-stack-ref-call3-shape ()
  "GNU CALL3's three compact STACK-REFs lower to BCL CALL3."
  (let* ((function
          (byte-compile '(lambda (first second third)
                           (nelisp-gnu-bytecode-vm-target3
                            first second third))))
         (signatures (make-hash-table :test #'eq))
         lowered)
    (puthash 'nelisp-gnu-bytecode-vm-target3 3 signatures)
    (should (= (aref function 0) 771))
    (should (equal (append (aref function 1) nil)
                   '(192 3 3 3 35 135)))
    (should (equal (append (aref function 2) nil)
                   '(nelisp-gnu-bytecode-vm-target3)))
    (setq lowered (nelisp-gnu-bytecode-vm--lower-function
                   function signatures (make-hash-table :test #'eq)))
    (should (equal (append (nelisp-bc-code lowered) nil)
                   '(1 0 2 3 2 3 2 3 30 3 0)))
    (should (= (nelisp-bc-stack-depth lowered) 7))))

(ert-deftest nelisp-gnu-bytecode-vm/exact-special-let-varbind-shape ()
  "GNU special LET lowers compact VARBIND/UNBIND to BCL specpdl ops."
  (defvar nelisp-gnu-bytecode-vm-counter 0)
  (let* ((function
          (byte-compile
           '(lambda ()
              (let ((nelisp-gnu-bytecode-vm-counter 7))
                (nelisp-gnu-bytecode-vm-target
                 nelisp-gnu-bytecode-vm-counter)))))
         (signatures (make-hash-table :test #'eq))
         lowered)
    (puthash 'nelisp-gnu-bytecode-vm-target 1 signatures)
    (setq lowered (nelisp-gnu-bytecode-vm--lower-function
                   function signatures (make-hash-table :test #'eq)))
    (should (equal (append (aref function 1) nil)
                   '(193 24 194 8 33 41 135)))
    (should (equal (append (nelisp-bc-code lowered) nil)
                   '(1 1 18 0 1 2 16 0 30 1 19 1 0)))
    (should (= (nelisp-bc-stack-depth lowered) 2))))

(ert-deftest nelisp-gnu-bytecode-vm/source-free-shared-stores-and-functions ()
  "GNU ELC top-level mutation and functions run in NeLisp's BCL stores."
  (let* ((dir (make-temp-file "nelisp-gnu-vm" t))
         (source (expand-file-name "fixture.el" dir))
         (elc (concat source "c"))
         (count 'nelisp-gnu-bytecode-vm-counter)
         (identity 'nelisp-gnu-bytecode-vm-identity)
         (plus-one 'nelisp-gnu-bytecode-vm-plus-one)
         (call-target 'nelisp-gnu-bytecode-vm-call-target)
         (special-call 'nelisp-gnu-bytecode-vm-special-call)
         (target 'nelisp-gnu-bytecode-vm-target)
         (target2 'nelisp-gnu-bytecode-vm-target2)
         (call-target2 'nelisp-gnu-bytecode-vm-call-target2)
         (target3 'nelisp-gnu-bytecode-vm-target3)
         (call-target3 'nelisp-gnu-bytecode-vm-call-target3)
         (object (cons 'identity nil)))
    (unwind-protect
        (progn
          (nelisp--reset)
          (copy-file nelisp-gnu-bytecode-vm-test--fixture source)
          (unless (funcall nelisp-gnu-bytecode-vm-test--host-byte-compile-file source)
            (ert-fail "GNU byte compiler did not produce fixture"))
          (delete-file source)
          (let ((gnu-call2
                 (nelisp-gnu-bytecode-vm-test--elc-function elc call-target2)))
            (should (byte-code-function-p gnu-call2))
            (should (= (aref gnu-call2 0) 514))
            (should (equal (append (aref gnu-call2 1) nil)
                           '(192 2 2 34 135)))
            (should (equal (append (aref gnu-call2 2) nil)
                           (list target2))))
          (let ((gnu-call3
                 (nelisp-gnu-bytecode-vm-test--elc-function elc call-target3)))
            (should (byte-code-function-p gnu-call3))
            (should (= (aref gnu-call3 0) 771))
            (should (equal (append (aref gnu-call3 1) nil)
                           '(192 3 3 3 35 135)))
            (should (equal (append (aref gnu-call3 2) nil)
                           (list target3))))
          (should (hash-table-p nelisp--functions))
          (should-error (nelisp-load-file elc) :type 'nelisp-load-error)
          (should (eq (gethash count nelisp--globals nelisp--unbound)
                      nelisp--unbound))
          (nelisp-gnu-bytecode-vm-load-file elc)
          (should (= (nelisp-eval count) 1))
          (should (nelisp-bcl-p (nelisp--function-of identity)))
          (should (nelisp-bcl-p (nelisp--function-of plus-one)))
          (should (nelisp-bcl-p (nelisp--function-of target)))
          (should (nelisp-bcl-p (nelisp--function-of call-target)))
          (should (nelisp-bcl-p (nelisp--function-of special-call)))
          (should (nelisp-bcl-p (nelisp--function-of target2)))
          (should (nelisp-bcl-p (nelisp--function-of call-target2)))
          (should (nelisp-bcl-p (nelisp--function-of target3)))
          (should (nelisp-bcl-p (nelisp--function-of call-target3)))
          (should (>= (nelisp-bc-stack-depth
                       (nelisp--function-of call-target)) 3))
          (should (eq (nelisp-eval (list identity (list 'quote object))) object))
          (should (= (nelisp-eval (list plus-one 41)) 42))
          (should (eq (nelisp-eval (list call-target (list 'quote object))) object))
          (garbage-collect)
          (should (eq (nelisp-eval (list call-target (list 'quote object))) object))
          (should (= (nelisp-eval (list special-call)) 77))
          (should (= (nelisp-eval (list call-target2 11 22)) 11))
          (should (= (nelisp-eval (list call-target3 11 22 33)) 11))
          (should (eq (nelisp-eval
                       (list call-target3 (list 'quote object) 22 33)) object))
          (garbage-collect)
          (should (eq (nelisp-eval
                       (list call-target3 (list 'quote object) 22 33)) object))
          (should (= (nelisp-eval count) 1))
          (nelisp--builtin-defalias
           target (nelisp-bc-make nil '(ignored) [42] [1 0 0] 2 0))
          (should (= (nelisp-eval (list call-target (list 'quote object))) 42))
          (nelisp--builtin-defalias
           target2 (nelisp-bc-make nil '(left right) [42] [1 0 0] 3 0))
          (should (= (nelisp-eval (list call-target2 11 22)) 42))
          (nelisp--builtin-defalias
           target3 (nelisp-bc-make nil '(first second third) [42] [1 0 0] 4 0))
          (should (= (nelisp-eval (list call-target3 11 22 33)) 42))
          (nelisp--builtin-defalias
           target2 (nelisp-bc-make nil '(left right) [] [255] 2 0))
          (should-error (nelisp-eval (list call-target2 11 22))
                        :type 'nelisp-bc-error)
          (nelisp--builtin-defalias
           target3 (nelisp-bc-make nil '(first second third) [] [255] 3 0))
          (should-error (nelisp-eval (list call-target3 11 22 33))
                        :type 'nelisp-bc-error)
          (nelisp--builtin-defalias
           target (nelisp-bc-make nil '(argument) [] [255] 1 0))
          (should-error (nelisp-eval (list special-call))
                        :type 'nelisp-bc-error)
          (should (= (nelisp-eval count) 1))
          (should (nelisp-load--feature-provided-p
                   'nelisp-gnu-bytecode-vm-shared-store-fixture)))
      (nelisp--reset)
      (when (file-exists-p source) (delete-file source))
      (when (file-exists-p elc) (delete-file elc))
      (delete-directory dir t))))

(ert-deftest nelisp-gnu-bytecode-vm/forward-call3-refused-before-effects ()
  "A later CALL3 target cannot bypass whole-file preflight."
  (let* ((dir (make-temp-file "nelisp-gnu-vm-forward3" t))
         (source (expand-file-name "forward3.el" dir))
         (elc (concat source "c"))
         (counter 'nelisp-gnu-bytecode-vm-forward3-counter)
         (caller 'nelisp-gnu-bytecode-vm-forward3-caller)
         (target 'nelisp-gnu-bytecode-vm-forward3-target)
         (stale-target
          (nelisp-bc-make nil '(first second third) [42] [1 0 0] 4 0)))
    (unwind-protect
        (progn
          (nelisp--reset)
          (nelisp--builtin-defalias target stale-target)
          (with-temp-file source
            (insert (format
                     ";;; -*- lexical-binding: t; -*-\n(defvar %s 0)\n(setq %s (1+ %s))\n(declare-function %s nil)\n(defun %s (first second third) (%s first second third))\n(defun %s (first second third) 42)\n"
                     counter counter counter target caller target target)))
          (unless (funcall nelisp-gnu-bytecode-vm-test--host-byte-compile-file
                           source)
            (ert-fail "GNU byte compiler did not emit CALL2 forward fixture"))
          (delete-file source)
          (let ((gnu-caller
                 (nelisp-gnu-bytecode-vm-test--elc-function elc caller))
                (error-data
                 (should-error (nelisp-gnu-bytecode-vm-load-file elc)
                               :type 'nelisp-gnu-bytecode-vm-error)))
            (should (equal (append (aref gnu-caller 1) nil)
                           '(192 3 3 3 35 135)))
            (should (eq (plist-get (cdr error-data) :reason)
                        'forward-call-target)))
          (should (eq (gethash counter nelisp--globals nelisp--unbound)
                      nelisp--unbound))
          (should (eq (gethash caller nelisp--functions nelisp--unbound)
                      nelisp--unbound))
          (should (eq (gethash target nelisp--functions nelisp--unbound)
                      stale-target)))
      (nelisp--reset)
      (when (file-exists-p source) (delete-file source))
      (when (file-exists-p elc) (delete-file elc))
      (delete-directory dir t))))

(ert-deftest nelisp-gnu-bytecode-vm/whole-file-preflight-blocks-partial-effects ()
  "Malformed and unsupported final forms leave no NeLisp global behind."
  (let* ((dir (make-temp-file "nelisp-gnu-vm-preflight" t))
         (elc (expand-file-name "bad.elc" dir))
         (counter 'nelisp-gnu-bytecode-vm-preflight-counter)
         (header (concat ";ELC" (string 31) "\n"))
         (inputs (list
                  (concat header "(defvar " (symbol-name counter)
                          " 1)\n(provide")
                  (concat header "(defvar " (symbol-name counter)
                          " 1)\n(byte-code \"\\77\\207\" [nil] 1)")
                  (concat header "(defvar " (symbol-name counter)
                          " 1)\n(provide 'late-feature extra)")
                  (concat header "(defvar " (symbol-name counter)
                          " 1)\n(byte-code \"\\300\\40\\207\" [nelisp-gnu-bytecode-vm-absent] 1)")
                  (concat header "(defvar " (symbol-name counter)
                          " 1)\n(byte-code \"\\300\\41\\207\" [nelisp-gnu-bytecode-vm-wrong-arity] 1)")
                  (concat header "(defvar " (symbol-name counter)
                          " 1)\n(byte-code \"\\300\\40\\207\" [nelisp-gnu-bytecode-vm-wrong-arity] 1)")
                  (concat header "(defvar " (symbol-name counter)
                          " 1)\n(defalias 'nelisp-gnu-bytecode-vm-bad-stack-ref (byte-code \"\\1\\207\" [] 1))")
                  (concat header "(defvar " (symbol-name counter)
                          " 1)\n(byte-code \"\\51\\207\" [] 1)")
                  (concat header "(defvar " (symbol-name counter)
                          " 1)\n(byte-code \"\\30\\207\" [symbol] 1)")
                  (concat header "(defvar " (symbol-name counter)
                          " 1)\n(byte-code \"\\301\\30\\207\" [symbol 7] 1)"))))
    (unwind-protect
        (progn
          (nelisp--reset)
          (nelisp--builtin-defalias
           'nelisp-gnu-bytecode-vm-wrong-arity
           (nelisp-bc-make nil '(argument) [] [0] 1 0))
          (dolist (text inputs)
            (with-temp-file elc (insert text))
            (should-error (nelisp-gnu-bytecode-vm-load-file elc)
                          :type 'nelisp-gnu-bytecode-vm-error)
            (should (eq (gethash counter nelisp--globals nelisp--unbound)
                        nelisp--unbound)))
          (should-error (nelisp-gnu-bytecode-vm--fixed-params '(x x))
                        :type 'nelisp-gnu-bytecode-vm-error))
      (nelisp--reset)
      (when (file-exists-p elc) (delete-file elc))
      (delete-directory dir t))))

(ert-deftest nelisp-gnu-bytecode-vm/forward-call-refused-before-effects ()
  "A CALL to a function defined later in the ELC fails before any action runs."
  (let* ((dir (make-temp-file "nelisp-gnu-vm-forward" t))
         (source (expand-file-name "forward.el" dir))
         (elc (concat source "c"))
         (counter 'nelisp-gnu-bytecode-vm-forward-counter)
         (caller 'nelisp-gnu-bytecode-vm-forward-caller)
         (target 'nelisp-gnu-bytecode-vm-forward-target)
         (stale-target
          (nelisp-bc-make nil nil [42] [1 0 0] 1 0)))
    (unwind-protect
        (progn
          (nelisp--reset)
          (nelisp--builtin-defalias target stale-target)
          (with-temp-file source
            (insert (format
                     ";;; -*- lexical-binding: t; -*-\n(defvar %s 0)\n(setq %s (1+ %s))\n(declare-function %s nil)\n(defun %s () (%s))\n(defun %s () 42)\n"
                     counter counter counter target caller target target)))
          (unless (funcall nelisp-gnu-bytecode-vm-test--host-byte-compile-file
                           source)
            (ert-fail "GNU byte compiler did not emit forward fixture"))
          (delete-file source)
          (should-error (nelisp-gnu-bytecode-vm-load-file elc)
                        :type 'nelisp-gnu-bytecode-vm-error)
          (should (eq (gethash counter nelisp--globals nelisp--unbound)
                      nelisp--unbound))
          (should (eq (gethash caller nelisp--functions nelisp--unbound)
                      nelisp--unbound))
          (should (eq (gethash target nelisp--functions nelisp--unbound)
                      stale-target)))
      (nelisp--reset)
      (when (file-exists-p source) (delete-file source))
      (when (file-exists-p elc) (delete-file elc))
      (delete-directory dir t))))

(ert-deftest nelisp-gnu-bytecode-vm/exact-two-argument-cons-shape ()
  "The pinned GNU CONS function lowers to BCL CONS with both argument refs."
  (let* ((function (make-byte-code 514 (unibyte-string 1 1 66 135) [] 4))
         (lowered (nelisp-gnu-bytecode-vm--lower-function
                   function (make-hash-table :test #'eq)
                   (make-hash-table :test #'eq))))
    (should (equal (append (nelisp-bc-code lowered) nil)
                   '(2 1 2 1 24 0)))
    (should (= (nelisp-bc-stack-depth lowered) 4))))

(ert-deftest nelisp-gnu-bytecode-vm/cons-stack-underflow-is-malformed ()
  "Opcode 66 must reject a malformed stream before constructing BCL."
  (let ((failure
         (condition-case err
             (progn
               (nelisp-gnu-bytecode-vm--lower-function
                (make-byte-code 0 (unibyte-string 66 135) [] 4)
                (make-hash-table :test #'eq)
                (make-hash-table :test #'eq))
               nil)
           (nelisp-gnu-bytecode-vm-error (error-message-string err)))))
    (should (string-match-p "stack-underflow" failure))))

(provide 'nelisp-gnu-bytecode-vm-test)
;;; nelisp-gnu-bytecode-vm-test.el ends here
