;;; nelisp-native-compile-speed-test.el --- Compile work reuse -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'ert)
(require 'cl-lib)
(require 'nelisp-aot-compiler)
(require 'nelisp-bytecode-native-rooted-cfg-shared-emit)
(require 'nelisp-bytecode-native-rooted-cfg-contract)

(defun p35--target-functions ()
  "Extract the same first genuine definitions used by the F3/U10 fixtures."
  (let ((names '(expand-file-name directory-files locate-file)) functions)
    (cl-labels ((walk (form)
                  (when (consp form)
                    (let ((name (cond ((eq (car form) 'defun) (cadr form))
                                      ((and (eq (car form) 'fset) (consp (cadr form)))
                                       (cadadr form)))))
                      (if (and (memq name names) (not (assq name functions)))
                          (push (cons name (byte-compile
                                           (if (eq (car form) 'defun)
                                               (cons 'lambda (cddr form))
                                             (caddr form)))) functions)
                        (let ((tail form))
                          (while (consp tail)
                            (walk (car tail)) (setq tail (cdr tail)))))))))
      (with-temp-buffer
        (insert-file-contents "scripts/nelisp-stdlib-prelude.el")
        (goto-char (point-min))
        (condition-case nil
            (while t (walk (read (current-buffer))))
          (end-of-file nil))))
    (should (= (length functions) 3))
    functions))

(ert-deftest p35/checked-input-reuse-equals-regeneration-for-three-targets ()
  (dolist (entry (p35--target-functions))
    (let ((function (cdr entry)) retained (frame-calls 0)
          (frame-owner (symbol-function 'nelisp-bytecode-frame-ir-build)))
      (cl-letf (((symbol-function 'nelisp-bytecode-frame-ir-build)
                 (lambda (&rest args)
                   (setq frame-calls (1+ frame-calls)) (apply frame-owner args))))
      (nelisp-bytecode-compiler-input-call-with-built
       function
       (lambda (input)
         (setq retained input)
         (let* ((reused (nelisp-bytecode-compiler-input--reuse-checked input))
                (regenerated (nelisp-bytecode-compiler-input-build function))
                (pair (nelisp-bytecode-native-rooted-cfg-shared-emit-build-from-input
                       input nelisp-bytecode-native-rooted-cfg-contract-shared-entry nil 'off))
                (contract (nelisp-bytecode-native-rooted-cfg-contract-create-shared-v2
                           input (plist-get pair :plan) (plist-get pair :emitted)))
                (_reset (setq frame-calls 0))
                (reconstruction (nelisp-bytecode-native-rooted-cfg-contract-valid-p
                                 contract :reconstruction input)))
           (should (= frame-calls 0))
           (should (eq reused input))
           (should (equal reused regenerated))
           (should reconstruction)
           (should (equal (plist-get reconstruction :input) regenerated))
           (should (equal (plist-get reconstruction :expected-contract) contract)))))
      (should-not (nelisp-bytecode-compiler-input--reuse-checked retained))))))

(ert-deftest p35/checked-input-reuse-refuses-mutation-and-restores-nested-scope ()
  (let ((function (byte-compile (lambda (x) (+ x 1)))) retained)
    (should-error
     (nelisp-bytecode-compiler-input-call-with-built
      function
      (lambda (input)
        (setq retained input)
        (should (eq input (nelisp-bytecode-compiler-input--reuse-checked input)))
        (nelisp-bytecode-compiler-input-call-with-built
         function (lambda (nested)
                    (should-not (nelisp-bytecode-compiler-input--reuse-checked input))
                    (should (eq nested (nelisp-bytecode-compiler-input--reuse-checked nested)))))
        (should (eq input (nelisp-bytecode-compiler-input--reuse-checked input)))
        (cl-letf (((symbol-function 'nelisp-bytecode-frame-ir-build) #'ignore))
          (should-not (nelisp-bytecode-compiler-input--reuse-checked input)))
        (let ((owner (symbol-function 'nelisp-bytecode-compiler-input--function-slots)))
          (cl-letf (((symbol-function 'nelisp-bytecode-compiler-input--function-slots)
                     (lambda (value) (funcall owner value))))
            (should-not (nelisp-bytecode-compiler-input--reuse-checked input))))
        (plist-put (plist-get input :frame-result) :max-stack-depth 999)
        (should-not (nelisp-bytecode-compiler-input--reuse-checked input))
        (error "scope cleanup probe")))
     :type 'error :exclude-subtypes t)
    (should-not (nelisp-bytecode-compiler-input--reuse-checked retained))
    (nelisp-bytecode-compiler-input-call-with-built
     function (lambda (input)
                (aset (aref function 1) 0 0)
                (should-not (nelisp-bytecode-compiler-input--reuse-checked input))))))

(ert-deftest p35/checked-input-refuses-builder-change-after-reconstruction ()
  (let ((function (byte-compile (lambda (x) (+ x 1)))))
    (nelisp-bytecode-compiler-input-call-with-built
     function
     (lambda (input)
       (let* ((pair (nelisp-bytecode-native-rooted-cfg-shared-emit-build-from-input
                     input nelisp-bytecode-native-rooted-cfg-contract-shared-entry nil 'off))
              (contract (nelisp-bytecode-native-rooted-cfg-contract-create-shared-v2
                         input (plist-get pair :plan) (plist-get pair :emitted)))
              (verified (nelisp-bytecode-native-rooted-cfg-contract-valid-p
                         contract :reconstruction input)))
         (should verified)
         (cl-letf (((symbol-function 'nelisp-bytecode-frame-ir-build)
                    (lambda (&rest _args) (list :status 'malformed :reason "changed builder"))))
           ;; A data-only comparison cannot detect changed verifier owners.
           (should (equal input (plist-get verified :input)))
           (should-not (nelisp-bytecode-compiler-input--reuse-checked input))
           (should-not (equal input (nelisp-bytecode-compiler-input-build function)))))))))

(ert-deftest p35/link-unit-parses-body-once-for-module-metadata ()
  (let ((owner (symbol-function 'nelisp-aot-compiler--parse)) (calls 0))
    (cl-letf (((symbol-function 'nelisp-aot-compiler--parse)
               (lambda (&rest arguments)
                 (setq calls (1+ calls)) (apply owner arguments))))
      (should (plist-get (nelisp-aot-compile-to-link-unit
                         '(defun speed_probe (x) (if (= x 0) 7 (+ x 1)))) :text))
      (should (= calls 1)))))

(ert-deftest p35/link-unit-reused-ir-keeps-root-metadata ()
  (let* ((source '(seq (defun make_str ((slot :type sexp) bytes len)
                        (sexp-write-str slot bytes len))))
         (expected (let ((nelisp-aot-compiler--allow-external-user-calls t))
                     (nelisp-aot-compiler--object-module-init-metadata source)))
         (unit (nelisp-aot-compile-to-link-unit source)))
    (should (plist-get (plist-get expected :plan) :root-descriptors))
    (should (equal (plist-get expected :bytes) (plist-get unit :rodata)))))

(ert-deftest p35/control-representation-proof-keeps-conservative-fallbacks ()
  (let ((fenv (list (list 'x :repr 'raw-i64)))
        (shared '(+ x 1)) (cycle (list 'seq)))
    (setcdr cycle cycle)
    (should-not (nelisp-aot-compiler--control-repr-fenv '(if x (+ x 1) 0) fenv))
    (should-not (nelisp-aot-compiler--control-repr-fenv
                 '(if x (let ((local 0)) (setq local 1)) 0) fenv))
    (dolist (form (list '(if x (setq x 1) 0) '(seq . malformed)
                       cycle (list 'if 'x shared shared)))
      (should (eq fenv (nelisp-aot-compiler--control-repr-fenv form fenv))))
    (dolist (name '(nelisp-aot-compiler--repr-snapshot
                    nelisp-aot-compiler--parse-multi-let
                    nelisp-aot-compiler--parse-extern-call-args))
      (let ((owner (symbol-function name)))
        (cl-letf (((symbol-function name)
                   (lambda (&rest arguments) (apply owner arguments))))
          (should (eq fenv (nelisp-aot-compiler--control-repr-fenv '(if x 1 0) fenv))))))))

(ert-deftest p35/control-representation-proof-matches-full-state-compile ()
  (dolist (body '((if (= x 0) (+ x 1) 7)
                  (if (= x 0) (setq x 3) (+ x 1))
                  (if (= x 0) (let ((x 0)) (setq x 9)) x)
                  (while (< x 4) (setq x (+ x 1)))
                  (cond ((= x 0) 3) (t (+ x 1)))
                  (and (= x 0) (+ x 1)) (or (= x 0) (+ x 1))))
    (let* ((source (list 'defun 'probe '(x) body))
           (optimized (nelisp-aot-compile-to-link-unit source))
           (reference
            (cl-letf (((symbol-function 'nelisp-aot-compiler--control-repr-fenv)
                       (lambda (_form fenv) fenv)))
              (nelisp-aot-compile-to-link-unit source))))
      (should (equal optimized reference)))))

(defun p35--with-printer-source (callback)
  "Temporarily load only the atom printer functions, leaving host primitives intact."
  (let* ((names '(nelisp--prn-chunks-add nelisp--prn-chunks-string
                  nelisp--prn-symbol-char-needs-escape-p
                  nelisp--prn-symbol-needs-leading-escape-p
                  nelisp--prn-symbol-escaped-uncached nelisp--prn-symbol-escaped))
         (owners (mapcar (lambda (name) (cons name (and (fboundp name) (symbol-function name)))) names)))
    (unwind-protect
        (progn
          (with-temp-buffer
            (insert-file-contents "scripts/nelisp-stdlib-prelude.el")
            (goto-char (point-min))
            (condition-case nil
                (while t
                  (let ((form (read (current-buffer))))
                    (when (and (eq (car-safe form) 'defun) (memq (cadr form) names))
                      (eval form t))))
              (end-of-file nil)))
          (funcall callback))
      (dolist (owner owners)
        (if (cdr owner) (fset (car owner) (cdr owner)) (fmakunbound (car owner)))))))

(defvar nelisp--prn-symbol-cache nil)
(declare-function nelisp--prn-symbol-escaped-uncached "nelisp-stdlib-prelude")
(declare-function nelisp--prn-symbol-escaped "nelisp-stdlib-prelude")
(declare-function nelisp--prn-symbol-needs-leading-escape-p "nelisp-stdlib-prelude")
(declare-function nelisp--prn-symbol-char-needs-escape-p "nelisp-stdlib-prelude")
(declare-function nelisp--prn-chunks-add "nelisp-stdlib-prelude")
(declare-function nelisp--prn-chunks-string "nelisp-stdlib-prelude")

(ert-deftest p35/atom-spans-preserve-reference-bytes-and-string-kind ()
  (p35--with-printer-source
   (lambda ()
     (cl-labels ((reference (s)
                   (if (= (length s) 0) "##"
                     (let ((chunks (cons nil nil)))
                       (when (nelisp--prn-symbol-needs-leading-escape-p s)
                         (nelisp--prn-chunks-add chunks "\\"))
                       (dotimes (i (length s))
                         (let ((c (aref s i)))
                           (when (nelisp--prn-symbol-char-needs-escape-p c)
                             (nelisp--prn-chunks-add chunks "\\"))
                           (nelisp--prn-chunks-add chunks (char-to-string c))))
                       (nelisp--prn-chunks-string chunks)))))
       (dolist (s (append '("" "." "123" "-1.2e3" "normal-name" "a\\b c" "電気λ😀")
                         (cl-loop for c below 256 append
                                  (list (concat "a" (string c) "b")
                                        (concat (unibyte-string 97 c 98))))))
         (let ((expected (reference s)) (actual (nelisp--prn-symbol-escaped-uncached s)))
           (should (equal actual expected))
           (should (eq (multibyte-string-p actual) (multibyte-string-p expected)))))))))

(ert-deftest p35/atom-cache-remains-bounded-and-owns-mutable-strings ()
  (p35--with-printer-source
   (lambda ()
     (let* ((nelisp--prn-symbol-cache (make-hash-table :test 'equal))
            (key (copy-sequence "ordinary-name"))
            (result (nelisp--prn-symbol-escaped key)))
       (aset key 0 ?X) (aset result 0 ?Y)
       (should (equal (nelisp--prn-symbol-escaped "ordinary-name") "ordinary-name"))
       (dotimes (i 1100) (nelisp--prn-symbol-escaped (format "name-%d" i)))
       (should (= (hash-table-count nelisp--prn-symbol-cache) 1024))))))

(declare-function string-byte "nelisp-stdlib-prelude")
(declare-function nelisp--check-string "nelisp-stdlib-prelude")
(defun p35--with-base64-source (callback)
  "Run CALLBACK with the real prelude encoder and host byte-access adapters."
  (let ((reference (symbol-function 'base64-encode-string)))
    (cl-letf (((symbol-function 'base64-encode-string) reference)
              ((symbol-function 'string-byte)
               (lambda (string index)
                 (aref (if (multibyte-string-p string)
                           (encode-coding-string string 'utf-8) string) index)))
              ((symbol-function 'nelisp--check-string)
               (lambda (value)
                 (unless (stringp value) (signal 'wrong-type-argument (list 'stringp value))))))
      (with-temp-buffer
        (insert-file-contents "scripts/nelisp-stdlib-prelude.el")
        (goto-char (point-min))
        (search-forward "(defun base64-encode-string")
        (goto-char (match-beginning 0))
        (eval (read (current-buffer)) t))
      (funcall callback reference))))

(ert-deftest p35/base64-fills-output-without-per-character-strings ()
  (p35--with-base64-source
   (lambda (_reference)
     (let ((owner (symbol-function 'char-to-string)) (calls 0))
       (cl-letf (((symbol-function 'char-to-string)
                  (lambda (&rest args) (setq calls (1+ calls)) (apply owner args))))
         (should (= (length (base64-encode-string
                            (string-make-unibyte (make-string 384 255)) t)) 512))
         (should (= calls 0)))))))

(ert-deftest p35/base64-keeps-padding-byte-order-and-existing-no-wrap-policy ()
  (p35--with-base64-source
   (lambda (reference)
     (dolist (s (append '("" "f" "fo" "foo" "foobar" "電気λ😀")
                       (cl-loop for n in '(1 2 3 127 128 129 383 384 385 1024)
                                collect (apply #'unibyte-string
                                               (cl-loop for i below n collect (mod (* i 73) 256))))))
       (let* ((bytes (if (multibyte-string-p s) (encode-coding-string s 'utf-8) s))
              (expected (funcall reference bytes t)))
         (dolist (flag '(nil t))
           (let ((actual (base64-encode-string s flag)))
             (should (equal actual expected))
             (should (eq (multibyte-string-p actual) (multibyte-string-p expected)))))))
     (should-error (base64-encode-string 7) :type 'wrong-type-argument))))

(declare-function nelisp--equal-recursive "nelisp-stdlib-prelude")
(ert-deftest p35/equal-shared-subtrees-do-not-repeat-the-native-walk ()
  (let ((native (symbol-function 'equal)) (calls 0))
    (cl-letf (((symbol-function 'nelisp--equal-recursive) nil)
              ((symbol-function 'nelisp--native-equal)
               (lambda (a b) (setq calls (1+ calls)) (funcall native a b))))
      (with-temp-buffer
        (insert-file-contents "scripts/nelisp-stdlib-prelude.el")
        (goto-char (point-min))
        (search-forward "(defun nelisp--equal-recursive")
        (goto-char (match-beginning 0))
        (eval (read (current-buffer)) t))
      (let* ((tree (cl-loop for i below 100 collect (list i (vector "text" i))))
             (snapshot (copy-tree tree t)))
        (should (nelisp--equal-recursive tree tree))
        (should (= calls 0))
        (should (nelisp--equal-recursive tree snapshot))
        (aset (cadar snapshot) 1 -1)
        (should-not (nelisp--equal-recursive tree snapshot))))))

(provide 'nelisp-native-compile-speed-test)
;;; nelisp-native-compile-speed-test.el ends here
