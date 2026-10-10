;;; Native boundary compiler regressions -*- lexical-binding: t; -*-
(require 'ert)
(require 'bytecomp)
(require 'nelisp-bytecode-native-rooted-cfg-shared-emit)
(require 'nelisp-bytecode-native-rooted-cfg-contract)

(defconst p35--equal-fixture
  (expand-file-name "nelisp-native-equal-word-fixture.el"
                    (file-name-directory (or load-file-name buffer-file-name))))

(defconst p35--vector-fixture
  (expand-file-name "nelisp-native-vm-vector-unit-fixture.el"
                    (file-name-directory p35--equal-fixture)))

(defconst p35--arithmetic-fixture
  (expand-file-name "nelisp-native-vm-arithmetic-unit-fixture.el"
                    (file-name-directory p35--equal-fixture)))

(defconst p35--difference-fixture
  (expand-file-name "nelisp-native-vm-builtin-difference-fixture.el"
                    (file-name-directory p35--equal-fixture)))

(defconst p35--marker-fixture
  (expand-file-name "nelisp-native-vm-marker-fixture.el"
                    (file-name-directory p35--equal-fixture)))

(defconst p35--cons-fixture
  (expand-file-name "nelisp-native-vm-cons-fixture.el"
                    (file-name-directory p35--equal-fixture)))

(defun p35--check-frame-fixture (file prefix marker count modes)
  "Run canonical frame code and require each mutation to fail at an assertion."
  (unless (and (eq system-type 'gnu/linux)
               (string-match-p "x86_64\\|amd64" system-configuration))
    (ert-skip "Requires a freestanding x86_64 Linux executable"))
  (let* ((default-directory
          (file-name-directory (directory-file-name (file-name-directory p35--equal-fixture))))
         (fixture (expand-file-name file (file-name-directory p35--equal-fixture)))
         (directory (make-temp-file "nelisp-frame-" t)))
    (unwind-protect
        (dolist (mode (cons "positive" modes))
          (let* ((output (expand-file-name mode directory))
                 (errors (concat output ".err"))
                 (process-environment (copy-sequence process-environment))
                 (expected (if (equal mode "positive") 0 2)))
            (setenv (concat prefix "_SOURCE") nil)
            (setenv (concat prefix "_VARIANT") mode)
            (setenv (concat prefix "_OUTPUT") output)
            (with-temp-buffer
              (should (= (call-process "timeout" nil (list (current-buffer) errors) nil
                                       "-k" "5" "120" (expand-file-name invocation-name invocation-directory)
                                       "-Q" "--batch" "-L" "lisp" "-L" "src" "-L" "scripts"
                                       "--load" fixture) 0))
              (should (string-match-p (format "%s.*checks=%d" marker count) (buffer-string))))
            (should (= (nth 7 (file-attributes errors)) 0))
            (with-temp-buffer
              (set-buffer-multibyte nil)
              (let ((coding-system-for-read 'no-conversion))
                (should (= (call-process "timeout" nil (list (current-buffer) errors) nil
                                         "-k" "2" "15" output) expected)))
              (should (= (buffer-size) 32))
              (let* ((bytes (buffer-string))
                     (failed (cl-loop for i below 8 sum (ash (aref bytes (+ 24 i)) (* i 8)))))
                (if (= expected 0) (should (= failed 0))
                  (should (<= 1 failed count)))))
            (should (= (nth 7 (file-attributes errors)) 0))))
      (delete-directory directory t))))

(ert-deftest p35/native-lexical-frame-preserves-jit-order-and-optional-rest ()
  (p35--check-frame-fixture
   "nelisp-native-vm-frame-fixture.el" "AF" "FRAME-NATIVE-COMPILED" 430
   '("bad-tag" "bad-frame" "bad-jit" "bad-descriptor" "bad-code-order"
     "bad-arity" "bad-optional" "bad-rest")))

(ert-deftest p35/native-dynamic-frame-preserves-binding-epoch-and-unwind ()
  (p35--check-frame-fixture
   "nelisp-native-vm-dynamic-frame-fixture.el" "OD" "DYNAMIC-NATIVE-COMPILED" 562
   '("bad-epoch" "bad-index" "bad-rest-start" "bad-optional" "bad-unbind"
     "bad-arity" "bad-exhaustion")))

(ert-deftest p35/native-rooted-cons-aliases-reject-value-and-allocation-mutants ()
  "Execute real constructor/cloner and VM arms with observable negative cases."
  (unless (and (eq system-type 'gnu/linux)
               (string-match-p "x86_64\\|amd64" system-configuration))
    (ert-skip "Requires a freestanding x86_64 Linux executable"))
  (let* ((default-directory
           (file-name-directory (directory-file-name (file-name-directory p35--equal-fixture))))
         (directory (make-temp-file "nelisp-cons-alias-" t)))
    (unwind-protect
        (dolist (mode '("positive" "bad-alias" "bad-header" "bad-order"
                        "bad-nil" "bad-tail" "old-allocation"))
          (let* ((expected (if (equal mode "positive") 0 2))
                 (output (expand-file-name mode directory)) (errors (concat output ".err"))
                 (process-environment (copy-sequence process-environment)))
            (setenv "AL_VARIANT" mode) (setenv "AL_OUTPUT" output)
            (with-temp-buffer
              (should (= (call-process "timeout" nil (list (current-buffer) errors) nil
                                       "-k" "5" "120" (expand-file-name invocation-name invocation-directory)
                                       "-Q" "--batch" "-L" "lisp" "-L" "src" "-L" "scripts"
                                       "--load" p35--cons-fixture) 0))
              (should (string-match-p "CONS-NATIVE-COMPILED.*checks=1194" (buffer-string))))
            (should (= (nth 7 (file-attributes errors)) 0))
            (with-temp-buffer
              (set-buffer-multibyte nil)
              (let ((coding-system-for-read 'no-conversion))
                (should (= (call-process "timeout" nil (list (current-buffer) errors) nil
                                         "-k" "2" "15" output) expected)))
              (should (= (buffer-size) 32))
              (let* ((bytes (buffer-string))
                     (words (cl-loop for offset from 0 below 32 by 8 collect
                                     (cl-loop for i below 8 sum
                                              (ash (aref bytes (+ offset i)) (* i 8))))))
                (if (= expected 0) (should (= (nth 3 words) 0))
                  (should (> (nth 3 words) 0)))))
            (should (= (nth 7 (file-attributes errors)) 0))))
      (delete-directory directory t))))

(ert-deftest p35/native-arity-markers-reject-value-and-allocation-mutants ()
  "Compare the actual marker predicates with their retained allocating forms."
  (unless (and (eq system-type 'gnu/linux)
               (string-match-p "x86_64\\|amd64" system-configuration))
    (ert-skip "Requires a freestanding x86_64 Linux executable"))
  (let* ((default-directory
           (file-name-directory (directory-file-name (file-name-directory p35--equal-fixture))))
         (directory (make-temp-file "nelisp-arity-markers-" t)))
    (unwind-protect
        (dolist (case '(("positive" 0) ("old-zero" 4)
                        ("bad-optional" 19) ("bad-rest" 55)))
          (let* ((mode (car case)) (expected (cadr case))
                 (output (expand-file-name mode directory)) (errors (concat output ".err"))
                 (process-environment (copy-sequence process-environment)))
            (setenv "AM_VARIANT" mode) (setenv "AM_OUTPUT" output)
            (with-temp-buffer
              (should (= (call-process "timeout" nil (list (current-buffer) errors) nil
                                       "-k" "5" "120" (expand-file-name invocation-name invocation-directory)
                                       "-Q" "--batch" "-L" "lisp" "-L" "src" "-L" "scripts"
                                       "--load" p35--marker-fixture) 0))
              (should (string-match-p "MARKER-UNIT-COMPILED.*checks=128" (buffer-string))))
            (should (= (nth 7 (file-attributes errors)) 0))
            (with-temp-buffer
              (set-buffer-multibyte nil)
              (let ((coding-system-for-read 'no-conversion))
                (should (= (call-process "timeout" nil (list (current-buffer) errors) nil
                                         "-k" "2" "15" output) expected)))
              (should (= (buffer-size) 16))
              (let* ((bytes (buffer-string))
                     (words (cl-loop for offset from 0 below 16 by 8 collect
                                     (cl-loop for i below 8 sum
                                              (ash (aref bytes (+ offset i)) (* i 8))))))
                (should (= (nth 1 words) expected))
                (when (= expected 0) (should (equal words '(0 0))))))
            (should (= (nth 7 (file-attributes errors)) 0))))
      (delete-directory directory t))))

(ert-deftest p35/native-live-builtin-difference-rejects-token-and-copy-mutants ()
  "Execute the actual live callable path, including unchanged fallback slots."
  (unless (and (eq system-type 'gnu/linux)
               (string-match-p "x86_64\\|amd64" system-configuration))
    (ert-skip "Requires a freestanding x86_64 Linux executable"))
  (let* ((default-directory
           (file-name-directory (directory-file-name (file-name-directory p35--equal-fixture))))
         (directory (make-temp-file "nelisp-live-difference-" t)))
    (unwind-protect
        (dolist (case '(("positive" 0) ("bad-head" 1) ("bad-name" 1)
                        ("bad-tail" 1) ("bad-argc" 1) ("bad-tag" 1)
                        ("bad-op" 1) ("bad-copy" 4) ("bad-allocation" 3)))
          (let* ((mode (car case)) (expected (cadr case))
                 (output (expand-file-name mode directory)) (errors (concat output ".err"))
                 (process-environment (copy-sequence process-environment)))
            (setenv "AD_VARIANT" mode) (setenv "AD_OUTPUT" output)
            (with-temp-buffer
              (should (= (call-process "timeout" nil (list (current-buffer) errors) nil
                                       "-k" "5" "120" (expand-file-name invocation-name invocation-directory)
                                       "-Q" "--batch" "-L" "lisp" "-L" "src" "-L" "scripts"
                                       "--load" p35--difference-fixture) 0))
              (should (string-match-p "CALL-DIFF-UNIT-COMPILED.*cases=321" (buffer-string))))
            (should (= (nth 7 (file-attributes errors)) 0))
            (with-temp-buffer
              (set-buffer-multibyte nil)
              (let ((coding-system-for-read 'no-conversion))
                (should (= (call-process "timeout" nil (list (current-buffer) errors) nil
                                         "-k" "2" "15" output) expected)))
              (should (= (buffer-size) 32))
              (let* ((bytes (buffer-string))
                     (words (cl-loop for offset from 0 below 32 by 8 collect
                                     (cl-loop for i below 8 sum
                                              (ash (aref bytes (+ offset i)) (* i 8))))))
                (should (= (nth 3 words) expected))
                (when (= expected 0)
                  (should (equal words '(0 1 0 0))))))
            (should (= (nth 7 (file-attributes errors)) 0))))
      (delete-directory directory t))))

(ert-deftest p35/native-vm-arithmetic-rejects-overflow-and-fallback-mutants ()
  "Execute actual VM arithmetic and its full-slot/fallback negative controls."
  (unless (and (eq system-type 'gnu/linux)
               (string-match-p "x86_64\\|amd64" system-configuration))
    (ert-skip "Requires a freestanding x86_64 Linux executable"))
  (let* ((default-directory
           (file-name-directory (directory-file-name (file-name-directory p35--equal-fixture))))
         (directory (make-temp-file "nelisp-vm-arithmetic-" t)))
    (unwind-protect
        (dolist (case '(("positive" 0) ("bad-input-range" 7) ("bad-input-wrap" 7)
                        ("bad-truth" 6) ("bad-nil" 6) ("bad-difference" 6)
                        ("bad-compare-boundary" 6)
                        ("bad-tag" 7) ("bad-copy" 6) ("bad-op" 6)
                        ("bad-product-bound" 7) ("bad-result-bound" 7)))
          (let* ((mode (car case)) (expected (cadr case))
                 (output (expand-file-name mode directory)) (errors (concat output ".err"))
                 (process-environment (copy-sequence process-environment)))
            (setenv "AA_VARIANT" mode) (setenv "AA_OUTPUT" output)
            (with-temp-buffer
              (should (= (call-process "timeout" nil (list (current-buffer) errors) nil
                                       "-k" "5" "120" (expand-file-name invocation-name invocation-directory)
                                       "-Q" "--batch" "-L" "lisp" "-L" "src" "-L" "scripts"
                                       "--load" p35--arithmetic-fixture) 0))
              (should (string-match-p
                       (format "COMPARE-UNIT-COMPILED.*cases=%d"
                               (if (equal mode "bad-input-wrap") 1 2614)) (buffer-string))))
            (should (= (nth 7 (file-attributes errors)) 0))
            (with-temp-buffer
              (set-buffer-multibyte nil)
              (let ((coding-system-for-read 'no-conversion))
                (should (= (call-process "timeout" nil (list (current-buffer) errors) nil
                                         "-k" "2" "15" output) expected)))
              (should (= (buffer-size) 24))
              (let* ((bytes (buffer-string))
                     (words (cl-loop for offset from 0 below 24 by 8 collect
                                     (cl-loop for i below 8 sum
                                              (ash (aref bytes (+ offset i)) (* i 8))))))
                (should (= (car words) 0))
                (should (= (nth 2 words) expected))
                (when (= expected 0) (should (= (nth 1 words) 2614)))))
            (should (= (nth 7 (file-attributes errors)) 0))))
      (delete-directory directory t))))

(ert-deftest p35/native-vm-vector-access-rejects-value-and-allocation-mutants ()
  "Execute actual VM vector access with bounds, bridge and copy controls."
  (unless (and (eq system-type 'gnu/linux)
               (string-match-p "x86_64\\|amd64" system-configuration))
    (ert-skip "Requires a freestanding x86_64 Linux executable"))
  (let* ((default-directory
           (file-name-directory (directory-file-name (file-name-directory p35--equal-fixture))))
         (directory (make-temp-file "nelisp-vm-vector-" t)))
    (unwind-protect
        (dolist (case '(("positive" 0) ("old-view" 6) ("bad-bounds" 61)
                        ("bad-bridge" 101) ("bad-shift" 21) ("bad-copy" 57)))
          (let* ((mode (car case)) (expected (cadr case))
                 (output (expand-file-name mode directory)) (errors (concat output ".err"))
                 (process-environment (copy-sequence process-environment)))
            (setenv "AV_VARIANT" mode) (setenv "AV_OUTPUT" output)
            (with-temp-buffer
              (should (= (call-process "timeout" nil (list (current-buffer) errors) nil
                                       "-k" "5" "120" (expand-file-name invocation-name invocation-directory)
                                       "-Q" "--batch" "-L" "lisp" "-L" "src" "-L" "scripts"
                                       "--load" p35--vector-fixture) 0))
              (should (string-match-p "ARRAY-UNIT-COMPILED.*checks=130" (buffer-string))))
            (should (= (nth 7 (file-attributes errors)) 0))
            (with-temp-buffer
              (set-buffer-multibyte nil)
              (let ((coding-system-for-read 'no-conversion))
                (should (= (call-process "timeout" nil (list (current-buffer) errors) nil
                                         "-k" "2" "15" output) expected)))
              (should (= (buffer-size) 32))
              (let* ((bytes (buffer-string))
                     (words (cl-loop for offset from 0 below 32 by 8 collect
                                     (cl-loop for i below 8 sum
                                              (ash (aref bytes (+ offset i)) (* i 8))))))
                (should (= (nth 3 words) expected))
                (when (= expected 0) (should (= (car words) 0)))))
            (should (= (nth 7 (file-attributes errors)) 0))))
      (delete-directory directory t))))

(ert-deftest p35/native-equal-tagged-words-preserve-values-and-reject-mutants ()
  "Execute production equality and its value/allocation negative controls."
  (unless (and (eq system-type 'gnu/linux)
               (string-match-p "x86_64\\|amd64" system-configuration))
    (ert-skip "Requires a freestanding x86_64 Linux executable"))
  (let* ((root (file-name-directory (directory-file-name
                                     (file-name-directory p35--equal-fixture))))
         (default-directory root)
         (directory (make-temp-file "nelisp-equal-word-" t)))
    (unwind-protect
        (dolist (case '(("old-zero" "0" 13) ("bad-shift" "0" 21)
                        ("bad-odd" "0" 21) ("bad-mixed-tag" "0" 21)
                        ("bad-cdr" "0" 21) ("bad-pointer" "0" 21)
                        ("positive" "0" 0) ("positive" "1" 0)))
          (let* ((mode (nth 0 case)) (tco (nth 1 case))
                 (expected (nth 2 case))
                 (output (expand-file-name (concat mode "-" tco) directory))
                 (errors (concat output ".err"))
                 (process-environment (copy-sequence process-environment)))
            (setenv "AUDIT_EQ_VARIANT" mode)
            (setenv "AUDIT_EQ_OUTPUT" output)
            (setenv "NELISP_TCO" tco)
            (with-temp-buffer
              (should (= (call-process "timeout" nil (list (current-buffer) errors) nil
                                       "-k" "5" "120" (expand-file-name invocation-name invocation-directory)
                                       "-Q" "--batch" "-L" "lisp" "-L" "src" "-L" "scripts"
                                       "--load" p35--equal-fixture) 0))
              (should (string-match-p "TAGGED-EQUAL-COMPILED.*cases=910" (buffer-string))))
            (with-temp-buffer
              (insert-file-contents errors)
              (if (equal tco "1")
                  (should (string-match-p
                           "\\`nelisp-tco: [0-9]+ rewrite(s) in unit tagged-equality\\.o\n\\'"
                           (buffer-string)))
                (should (= (buffer-size) 0))))
            (with-temp-buffer
              (set-buffer-multibyte nil)
              (let ((coding-system-for-read 'no-conversion))
                (should (= (call-process "timeout" nil (list (current-buffer) errors) nil
                                         "-k" "2" "15" output) expected)))
              (should (= (buffer-size) 32))
              (let* ((bytes (buffer-string))
                     (words (cl-loop for offset from 0 below 32 by 8 collect
                                     (cl-loop for i below 8 sum
                                              (ash (aref bytes (+ offset i)) (* i 8))))))
                (should (= (nth 0 words) 4))
                (should (= (nth 1 words) (if (equal mode "old-zero") 4 0)))
                (should (= (nth 3 words) expected))
                (when (= expected 0) (should (= (nth 2 words) 910)))))
            (should (= (nth 7 (file-attributes errors)) 0))))
      (delete-directory directory t))))

(ert-deftest p35/mutation-index-keeps-free-writes-and-conservative-fallback ()
  (require 'nelisp-aot-compiler)
  (let* ((outer (make-symbol "x")) (shadow (make-symbol "x"))
         (body `(seq (setq ,outer 1)
                     (let ((,shadow 0)) (setq ,shadow 2))
                     (quote (setq quoted 3))
                     (let ((bound 0)) (setq bound 4))))
         (index (nelisp-aot-compiler--multi-let-mutation-index body nil)))
    (should index)
    (should (gethash outer (cdr index)))
    (should-not (gethash shadow (cdr index)))
    (should-not (gethash 'quoted (cdr index)))
    (should-not (gethash 'bound (cdr index)))
    (should (eq index (nelisp-aot-compiler--multi-let-mutation-index body index))))
  (let* ((shared '(setq x 1)) (cycle (list 'seq)))
    (setcdr cycle cycle)
    (dolist (body (list '(seq . x) cycle (list 'seq shared shared)))
      (should-not (nelisp-aot-compiler--free-setq-index body)))))

(ert-deftest p35/flat-representation-snapshots-preserve-cells-order-and-joins ()
  "Compare source and GNU bytecode against the original list snapshot."
  (require 'nelisp-aot-compiler)
  (let* ((names '(nelisp-aot-compiler--repr-vector-size
                  nelisp-aot-compiler--repr-vector-p
                  nelisp-aot-compiler--repr-vector-list
                  nelisp-aot-compiler--repr-snapshot
                  nelisp-aot-compiler--repr-restore
                  nelisp-aot-compiler--repr-vector-join-fixes
                  nelisp-aot-compiler--repr-join-fixes
                  nelisp-aot-compiler--repr-boxed-cells
                  nelisp-aot-compiler--repr-raw-cells))
         (owners (mapcar (lambda (name) (cons name (symbol-function name))) names)))
    (unwind-protect
        (dolist (compiled '(nil t))
          (dolist (entry owners)
            (fset (car entry) (if compiled (byte-compile (cdr entry)) (cdr entry))))
          (cl-labels
              ((legacy (fenv)
                 (let (result)
                   (dolist (cell fenv)
                     (when (consp cell)
                       (push (cons cell (plist-get (cdr cell) :repr)) result)))
                   result))
               (as-list (snapshot)
                 (if (nelisp-aot-compiler--repr-vector-p snapshot)
                     (nelisp-aot-compiler--repr-vector-list snapshot) snapshot))
               (changed-spine (function mode)
                 (let* ((fenv (cl-loop for i below 17 collect (list i :repr 'raw-i64)))
                        (lookup (symbol-function 'plist-get)) (called nil))
                   (cl-letf (((symbol-function 'plist-get)
                              (lambda (plist key)
                                (unless called
                                  (setq called t)
                                  (pcase mode
                                    ('shrink (setcdr fenv nil))
                                    ('grow (setcdr (last fenv) (list (list 17 :repr 'sexp-ptr))))
                                    ('atom (setcar (cdr fenv) 'removed))))
                                (funcall lookup plist key))))
                     (as-list (funcall function fenv))))))
            (dolist (size '(0 1 16 17 40 321))
              (let* ((cells (cl-loop for i below size collect
                                     (list (make-symbol "shadow") :slot i :repr
                                           (nth (% i 3) '(raw-i64 sexp-ptr unknown)))))
                     (old (legacy cells))
                     (snapshot (nelisp-aot-compiler--repr-snapshot cells)))
                (should (equal (as-list snapshot) old))
                (should (eq (and (> size 16) t)
                            (and (nelisp-aot-compiler--repr-vector-p snapshot) t)))
                (dolist (cell cells)
                  (setcdr cell (plist-put (cdr cell) :repr 'unknown))
                  (setcdr cell (plist-put (cdr cell) :root-p t)))
                (when cells (setcar cells (list 'replacement :repr 'sexp-ptr)))
                (nelisp-aot-compiler--repr-restore snapshot)
                (dolist (entry old)
                  (should (eq (plist-get (cdar entry) :repr) (cdr entry)))
                  (should (plist-get (cdar entry) :root-p)))))
            ;; Duplicate identities may have different recorded values.
            ;; A forced hash collision must still choose the first B entry.
            (let* ((cells (cl-loop for i below 40 collect (list (make-symbol "same") :repr 'raw-i64)))
                   (a (nelisp-aot-compiler--repr-snapshot (append cells (list (car cells)))))
                   (b (copy-sequence a)))
              (cl-loop for i from 2 below (length b) by 2 do (aset b i 'sexp-ptr))
              (aset b (- (length b) 1) 'raw-i64)
              (let* ((la (as-list a)) (lb (as-list b))
                     (expected (nelisp-aot-compiler--repr-join-fixes la lb)))
                (cl-letf (((symbol-function 'sxhash-eq) (lambda (_) 0)))
                  (should (equal (nelisp-aot-compiler--repr-join-fixes a b) expected))
                  (should (equal (nelisp-aot-compiler--repr-join-fixes a lb) expected))
                  (should (equal (nelisp-aot-compiler--repr-join-fixes la b) expected)))
                (should (equal (nelisp-aot-compiler--repr-boxed-cells (list a b))
                               (nelisp-aot-compiler--repr-boxed-cells (list la lb))))
                (should (equal (nelisp-aot-compiler--repr-raw-cells a cells)
                               (nelisp-aot-compiler--repr-raw-cells la cells)))))
            (let* ((cells (cl-loop for i below 40 collect (list i :repr 'raw-i64)))
                   (lookup (symbol-function 'plist-get)) (trace nil) snapshot)
              (cl-letf (((symbol-function 'plist-get)
                         (lambda (plist key)
                           (when (eq key :repr) (push plist trace))
                           (funcall lookup plist key))))
                ;; Trace objects, including each shadowed cell, rather than
                ;; names; compiler callbacks see the original lookup order.
                (setq snapshot (nelisp-aot-compiler--repr-snapshot cells)))
              (should (= (length trace) 40))
              (should (cl-every #'eq trace (reverse (mapcar #'cdr cells))))
              (should (equal (as-list snapshot) (legacy cells))))
            (dolist (mode '(shrink grow atom))
              (should (equal (changed-spine #'legacy mode)
                             (changed-spine #'nelisp-aot-compiler--repr-snapshot mode))))
            (let* ((cycle (list (list 'x :repr 'raw-i64)))
                   (dotted (cons (car cycle) 'tail)))
              (setcdr cycle cycle)
              (should-not (nelisp-aot-compiler--repr-vector-size cycle))
              (should-not (nelisp-aot-compiler--repr-vector-size dotted))
              (should (equal (nelisp-aot-compiler--repr-snapshot '(nil 2 (x :repr raw-i64)))
                             (legacy '(nil 2 (x :repr raw-i64))))))))
      (dolist (entry owners) (fset (car entry) (cdr entry))))))

(ert-deftest p35/mutation-index-observes-parser-callback-body-changes ()
  (require 'nelisp-aot-compiler)
  (cl-labels
      ((run (legacy)
         (let* ((body (copy-tree '(seq (setq x 1) 0)))
                (nelisp-aot-compiler--next-rt-let-slot (list 0))
                (owner (symbol-function 'nelisp-aot-compiler--parse-let-var))
                (getter (symbol-function 'nelisp-aot-compiler--multi-let-mutation-index))
                (calls 0))
           (cl-letf (((symbol-function 'nelisp-aot-compiler--parse-let-var)
                      (lambda (&rest args)
                        (when (= (setq calls (1+ calls)) 6)
                          (setcdr (last body) (list '(setq y 1))))
                        (apply owner args)))
                     ((symbol-function 'nelisp-aot-compiler--multi-let-mutation-index)
                      (if legacy (lambda (&rest _) nil) getter)))
             (let ((result (nelisp-aot-compiler--parse-multi-let
                            '((x 0) (y 0)) body nil nil nil
                            #'nelisp-aot-compiler--parse-value)))
               (should (>= calls 6))
               result)))))
    (should (equal (run t) (run nil)))))

(ert-deftest p34/optional-rest-preserve-required-and-normalized-layout ()
  (dolist (source '((lambda (a &optional b c) (list a b c))
                    (lambda (a &optional b &rest c) (list a b c))))
    (let* ((input (nelisp-bytecode-compiler-input-build (byte-compile source)))
           (plan (nelisp-bytecode-native-rooted-cfg-plan input)))
      (should (eq (plist-get plan :status) 'complete))
      (should (= (plist-get input :argument-min) 1))
      (should (= (plist-get plan :arity) 3))
      (should-not (plist-get input :argument-count)))))

(ert-deftest p35/banked-entry-and-call-staging-fit-large-cyclic-function ()
  (let* ((source `(lambda (n) (let ((sum 0))
                    (while (> n 0)
                      ,@(cl-loop for i from 1 to 20
                                 collect `(when (> n ,i) (setq sum (+ sum ,i))))
                      (setq n (1- n))) sum)))
         (input (nelisp-bytecode-compiler-input-build (byte-compile source)))
         (plan (nelisp-bytecode-native-rooted-cfg-plan input))
         (entry-banks (mapcar (lambda (block) (mapcar (lambda (phi) (plist-get phi :root))
                                                      (plist-get block :phis)))
                             (plist-get plan :blocks)))
         (staging (cl-loop for block in (plist-get plan :blocks)
                           append (cl-loop for op in (plist-get block :operations)
                                           when (plist-get op :staging-roots)
                                           collect (plist-get op :staging-roots)))))
    (should (eq (plist-get plan :status) 'complete))
    (should (plist-get plan :banked))
    (should (< (plist-get plan :required-root-count) 256))
    (should (> (length entry-banks) 20))
    (should (< (length (delete-dups (apply #'append entry-banks))) 5))
    (should (= (length (delete-dups (mapcar #'car staging))) 1))
    (let ((emitted (nelisp-bytecode-native-rooted-cfg-shared-emit-build
                    plan nelisp-bytecode-native-rooted-cfg-contract-shared-entry)))
      (should (eq (plist-get emitted :status) 'complete))
      (should (nelisp-bytecode-native-rooted-cfg-contract-create-shared-v2 input plan emitted)))))

(defun p35--run-raw-emission (emission environment)
  "Execute the validated scalar CFG grammar using GNU evaluation of expressions."
  (let* ((body (nth 3 (plist-get emission :form)))
         (bindings (append (mapcar (lambda (binding) (cons (car binding) (cadr binding)))
                                   (cadr body))
                           (copy-tree environment)))
         (cfg (caddr body))
         (blocks (nelisp-native-cfg-grammar-validate cfg))
         (label (nth 2 cfg)) (steps 0))
    (catch 'returned
      (while t
        (when (> (setq steps (1+ steps)) 100) (error "Nonterminating test CFG"))
        (let* ((block (cl-find label blocks :key #'cadr :test #'equal))
               (term (nth 3 block)))
          (unless block (error "Missing test CFG label"))
          (dolist (form (nth 2 block)) (eval form bindings))
          (pcase (car term)
            ('jump (setq label (cadr term)))
            ('branch (setq label (if (eval (cadr term) bindings) (nth 2 term) (nth 3 term))))
            ('return (throw 'returned (list (eval (cadr term) bindings)
                                           (cdr (assq 'effect bindings)))))
            (_ (error "Unexpected test CFG terminator"))))))))

(ert-deftest p35/shared-node-keeps-scope-and-effects-under-identity-hash-collisions ()
  ;; The two arms contain the SAME continuation cons object, but bind X to
  ;; different locals. Sharing merely by AST identity would return 523 twice.
  (let* ((shared '(progn (setq effect (+ effect 1)) (+ 512 x)))
         (body `(if env (let ((x 11)) ,shared) (let ((x 22)) ,shared)))
         (plan '(:blocks ((:start 0 :successors nil)) :arity 1 :required-root-count 0))
         emission)
    (cl-letf (((symbol-function 'nelisp-bytecode-native-rooted-cfg-shared-emit--operations)
               (lambda (&rest _) body))
              ((symbol-function 'sxhash-eq) (lambda (_) 0)))
      (setq emission (nelisp-bytecode-native-rooted-cfg-shared-emit--cycles plan "p35_scope_test")))
    (should (eq (plist-get emission :status) 'complete))
    (dolist (case '((t 523) (nil 534)))
      (should (equal (p35--run-raw-emission
                      emission (list (cons 'env (car case)) '(argument-count . 1)
                                     '(root-count . 0) '(effect . 0)))
                     (list (cadr case) 1))))))

(ert-deftest p35/no-captured-mutation-preserves-read-only-ast ()
  (require 'nelisp-aot-compiler)
  (let* ((branch '(seq (+ x 1) (setq y (+ y 2))))
         (form `(if flag ,branch (let ((x 3)) ,branch)))
         (before (copy-tree form)))
    (should (eq form (nelisp-aot-compiler--rewrite-frame-slot-refs form nil)))
    (should (equal form before))))

(ert-deftest p35/captured-mutation-still-rewrites-free-references ()
  (require 'nelisp-aot-compiler)
  (let* ((form '(seq x (setq x (+ x 1))
                    (let ((x (+ x 2))) x)
                    (let ((y x)) (+ x y))
                    (quote x) (function x)))
         (before (copy-tree form)))
    (should (equal (nelisp-aot-compiler--rewrite-frame-slot-refs form '(x))
                   '(seq (aot-frame-slot-ref 'x)
                         (setq x (+ (aot-frame-slot-ref 'x) 1))
                         (let ((x (+ (aot-frame-slot-ref 'x) 2))) x)
                         (let ((y (aot-frame-slot-ref 'x)))
                           (+ (aot-frame-slot-ref 'x) y))
                         (quote x) (function x))))
    (should (equal form before))))

(ert-deftest p35/representation-join-keeps-first-match-and-shadowed-cells ()
  (require 'nelisp-aot-compiler)
  (let* ((outer (cons 'x nil)) (inner (cons 'x nil))
         (b (append (list (cons outer 'raw-i64) (cons outer 'sexp-ptr)
                          (cons inner 'sexp-ptr))
                    (cl-loop repeat 20 collect (cons (cons 'other nil) 'unknown))))
         (a (list (cons outer 'sexp-ptr) (cons inner 'raw-i64))))
    (cl-letf (((symbol-function 'sxhash-eq) (lambda (_) 0)))
      (should (equal (nelisp-aot-compiler--repr-join-fixes a b)
                     (list (cons inner 'a) (cons outer 'b)))))))

(ert-deftest p35/representation-join-preserves-unknown-and-fallback ()
  (require 'nelisp-aot-compiler)
  (let* ((cell (cons 'x nil))
         (padding (cl-loop repeat 20 collect (cons (cons 'other nil) 'unknown))))
    (dolist (pair '((nil raw-i64) (unknown sexp-ptr) (raw-i64 unknown)
                   (sexp-ptr nil) (raw-i64 raw-i64) (sexp-ptr sexp-ptr)))
      (should-not (nelisp-aot-compiler--repr-join-fixes
                   (list (cons cell (car pair)))
                   (cons (cons cell (cadr pair)) padding))))
    ;; ASSQ can find this head before inspecting a malformed dotted tail.
    (should (equal (nelisp-aot-compiler--repr-join-fixes
                    (list (cons cell 'sexp-ptr))
                    (cons (cons cell 'raw-i64) 'dotted))
                   (list (cons cell 'b))))))

(ert-deftest p35/representation-join-terminates-on-cyclic-early-match ()
  (require 'nelisp-aot-compiler)
  (let* ((cell (cons 'x nil)) (b (list (cons cell 'raw-i64))))
    (setcdr b b)
    (should-not (nelisp-aot-compiler--repr-join-fixes nil b))
    (should (equal (nelisp-aot-compiler--repr-join-fixes
                    (list (cons cell 'sexp-ptr)) b)
                   (list (cons cell 'b))))))
