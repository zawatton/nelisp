;;; nelisp-aot-lisp-arith-test.el --- 1+/1- and word-typed fixnum tests in native artifacts  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Native artifacts of Lisp source lower (1+ X) and (1- X) as (+ X 1) and
;; (- X 1), as GNU's Badd1/Bsub1 are Bplus/Bdiff with the constant 1, and
;; test a value word for a fixnum with the mask test (logand w 3) = 1.
;; The answers must stay GNU's: an overflow past `most-positive-fixnum'
;; is the bignum, a float or marker operand and a wrong type take the
;; generic function.
;;
;; The compiler runs here, on the host; the native code runs inside the
;; NeLisp runtime image.  The differential test needs that image, so it
;; runs only when NELISP_AOT_DIFF_HOME (a checkout with target/nelisp and
;; build/nemacs-runtime.nlri) is set; it takes about 100 s.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-artifact)
(require 'nelisp-aot-compiler)
(load (expand-file-name "nelisp-aot-lisp-arith-cases.el"
                        (file-name-directory
                         (or load-file-name buffer-file-name)))
      nil t)

(defvar nelisp-aot-lisp-arith-test--dir
  (file-name-directory (directory-file-name
                        (file-name-directory
                         (or load-file-name buffer-file-name)))))

(defun nelisp-aot-lisp-arith-test--gnu-lines ()
  "Return GNU's lines for the cases, from the byte-compiled source."
  (let ((pos 0) (r nil))
    (while (setq r (condition-case nil
                       (read-from-string nelisp-aot-lisp-arith-cases-source pos)
                     (end-of-file nil)))
      (eval (car r) nil)
      (setq pos (cdr r))))
  (dolist (n nelisp-aot-lisp-arith-cases-names) (byte-compile (intern n)))
  (nelisp-aot-lisp-arith-cases-run (lambda (n a) (apply (intern n) a))))

;; Error data is compared too: the loader now re-signals a native call's
;; condition and data (native-load reads the pending exit's WORDs).
;; One known runtime divergence is allowed, narrowly: the image's own
;; comparison builtins (the interpreter too: (< 2305843009213693952 'a))
;; name a BIGNUM first operand as the culprit where GNU names the bad
;; operand.  It lives in the runtime's comparison builtin, not in the
;; lowering.  A line differs acceptably only when it is a comparison
;; function's wrong-type-argument whose native culprit is a bignum literal;
;; every other line, and every other part of those lines, compares exactly.
(defun nelisp-aot-lisp-arith-test--lines-equal (want got)
  (and (= (length want) (length got))
       (cl-every
        (lambda (w g)
          (or (equal w g)
              (and (string-match
                    "\\`\\(lad-\\(?:lt\\|le\\|gt\\|eq\\)-[a-z]+ ([^)]*) => (signal wrong-type-argument (number-or-marker-p \\)\\(.*\\)))\\'" g)
                   (let ((prefix (match-string 1 g)) (culprit (match-string 2 g)))
                     (and (string-match-p "\\`-?[0-9]\\{19,\\}\\'" culprit)
                          (string-prefix-p prefix w))))))
        want got)))

(defun nelisp-aot-lisp-arith-test--compile (source dir name)
  "Compile SOURCE into DIR/NAME.neln and return the path."
  (let ((el (expand-file-name (concat name ".el") dir))
        (neln (expand-file-name (concat name ".neln") dir)))
    (write-region source nil el nil 'silent)
    (nelisp-artifact-compile-file el neln nil nil nil nil nil 'neln 'opportunistic)
    neln))

(defun nelisp-aot-lisp-arith-test--image-lines (neln home)
  "Run the cases natively from NELISP-LOADED NELN in the image under HOME."
  (let* ((dir (file-name-directory neln))
         (script (expand-file-name "run.el" dir))
         (lisp (expand-file-name "lisp" nelisp-aot-lisp-arith-test--dir))
         (src (expand-file-name "src" nelisp-aot-lisp-arith-test--dir))
         (cases (expand-file-name "test/nelisp-aot-lisp-arith-cases.el"
                                  nelisp-aot-lisp-arith-test--dir)))
    (write-region
     (format "(setq load-path (append (list %S %S) load-path))
(require 'nelisp-native-load)
(load %S nil t t)
(defvar dt-handles nil)
(defun dt-call (name args)
  (let ((h (cdr (assoc name dt-handles))))
    (unless h
      (setq h (nelisp-native-load-artifact %S name))
      (setq dt-handles (cons (cons name h) dt-handles)))
    (nelisp-native-load-call h args)))
(dolist (l (nelisp-aot-lisp-arith-cases-run (function dt-call))) (princ \"L \") (princ l) (princ \"\\n\"))
(princ \"DT-DONE\\n\")\n"
             lisp src cases neln)
     nil script nil 'silent)
    (with-temp-buffer
      (let ((default-directory (file-name-as-directory home))
            (process-environment (cons (concat "NELISP_HOME=" home)
                                       process-environment)))
        (call-process "bash" nil t nil "-c"
                      (let ((dump (getenv "NELISP_AOT_FLAT_DUMP")))
                        (if (and dump (not (string-empty-p dump)))
                            ;; Flat heap-snapshot boot (no .nlri needed).
                            (format "ulimit -v 8388608; exec timeout 900 taskset -c 6,22 target/nelisp --cold-load-from %s --eval %s < /dev/null"
                                    (shell-quote-argument dump)
                                    (shell-quote-argument
                                     (format "(progn (nemacs-runtime-dump-reinit) (load %S nil t t))" script)))
                          (format "ulimit -v 8388608; exec timeout 900 target/nelisp exec-runtime-image build/nemacs-runtime.nlri '(load \"%s\" nil t t)' < /dev/null"
                                  script)))))
      (goto-char (point-min))
      (should (search-forward "DT-DONE" nil t))
      (let (out)
        (goto-char (point-min))
        (while (re-search-forward "^L \\(.*\\)$" nil t)
          (push (match-string 1) out))
        (nreverse out)))))

(defun nelisp-aot-lisp-arith-test--home ()
  "Return NELISP_AOT_DIFF_HOME when set and usable, nil when unset.
A set but unusable value is an error, never a silent skip: the image
differential would otherwise report green without having run."
  (let ((h (getenv "NELISP_AOT_DIFF_HOME")))
    (when (and h (not (string-empty-p h)))
      (unless (file-executable-p (expand-file-name "target/nelisp" h))
        (error "NELISP_AOT_DIFF_HOME=%s has no executable target/nelisp" h))
      (unless (or (let ((d (getenv "NELISP_AOT_FLAT_DUMP"))) (and d (file-readable-p d)))
                  (file-readable-p (expand-file-name "build/nemacs-runtime.nlri" h)))
        (error "NELISP_AOT_DIFF_HOME=%s has no build/nemacs-runtime.nlri" h))
      h)))

(ert-deftest nelisp-aot-lisp-arith/lowering-has-no-per-call-builtin ()
  "A native loop on 1+ carries no one-argument builtin delegation."
  (skip-unless (and (eq system-type 'gnu/linux)
                    (string-match-p "x86_64" (or system-configuration ""))))
  (let ((dir (make-temp-file "lad-" t)))
    (unwind-protect
        (let* ((neln (nelisp-aot-lisp-arith-test--compile
                      "(defun lad-l (n) (let ((i 0)) (while (< i n) (setq i (1+ i))) i))\n"
                      dir "l"))
               (native (plist-get (nelisp-artifact-read-manifest neln) :native)))
          (should (member "lad-l" (plist-get native :symbols)))
          (should-not (member "nelisp_aot_funcall1" (plist-get native :externs))))
      (delete-directory dir t))))

(ert-deftest nelisp-aot-lisp-arith/image-differential-vs-gnu ()
  "Native answers near the fixnum boundary equal GNU's, in the runtime image."
  (skip-unless (nelisp-aot-lisp-arith-test--home))
  (let ((dir (make-temp-file "lad-" t)))
    (unwind-protect
        (let* ((neln (nelisp-aot-lisp-arith-test--compile
                      nelisp-aot-lisp-arith-cases-source dir "all"))
               (want (nelisp-aot-lisp-arith-test--gnu-lines))
               (got (nelisp-aot-lisp-arith-test--image-lines
                     neln (nelisp-aot-lisp-arith-test--home))))
          (should (nelisp-aot-lisp-arith-test--lines-equal want got)))
      (delete-directory dir t))))

(ert-deftest nelisp-aot-lisp-arith/image-differential-mutation-control ()
  "A broken lowering (no overflow check on +) is caught by the differential."
  (skip-unless (nelisp-aot-lisp-arith-test--home))
  (let ((dir (make-temp-file "lad-" t))
        (orig (symbol-function 'nelisp-aot-compiler--lisp-fixnum-result-form)))
    (unwind-protect
        (let* ((neln (cl-letf (((symbol-function
                                 'nelisp-aot-compiler--lisp-fixnum-result-form)
                                (lambda (op a b fallback &optional word)
                                  (if (eq op '+)
                                      (if word
                                          `(nelisp-aot--raw-as-word
                                            (logior (shl (nelisp-aot--raw-op + ,a ,b) 2) 1))
                                        `(nelisp-aot--raw-op + ,a ,b))
                                    (funcall orig op a b fallback word)))))
                       (nelisp-aot-lisp-arith-test--compile
                        nelisp-aot-lisp-arith-cases-source dir "mut")))
               (want (nelisp-aot-lisp-arith-test--gnu-lines))
               (got (nelisp-aot-lisp-arith-test--image-lines
                     neln (nelisp-aot-lisp-arith-test--home))))
          (should-not (nelisp-aot-lisp-arith-test--lines-equal want got)))
      (delete-directory dir t))))

;; Further mutants of the lowering.  Each is a deliberate defect in one
;; guard; the in-image differential must notice (lines differ from GNU).
(defun nelisp-aot-lisp-arith-test--mutant-detected (mutate tag)
  "Compile the cases with MUTATE (a function taking a thunk) and return non-nil
when the image's answers differ from GNU's, or the mutant does not even finish."
  (let ((dir (make-temp-file "lad-" t)))
    (unwind-protect
        (let* ((neln (funcall mutate
                              (lambda ()
                                (nelisp-aot-lisp-arith-test--compile
                                 nelisp-aot-lisp-arith-cases-source dir tag))))
               (want (nelisp-aot-lisp-arith-test--gnu-lines))
               (got (condition-case nil
                        (nelisp-aot-lisp-arith-test--image-lines
                         neln (nelisp-aot-lisp-arith-test--home))
                      ;; A crash before DT-DONE also counts as detected.
                      (error :crashed))))
          (or (eq got :crashed)
              (not (nelisp-aot-lisp-arith-test--lines-equal want got))))
      (delete-directory dir t))))

(ert-deftest nelisp-aot-lisp-arith/mutant-range-check ()
  "Control: a range check that always passes is caught."
  (skip-unless (nelisp-aot-lisp-arith-test--home))
  (should (nelisp-aot-lisp-arith-test--mutant-detected
           (lambda (thunk)
             (cl-letf (((symbol-function 'nelisp-aot-compiler--lisp-fixnum-range-form)
                        (lambda (_v) '(nelisp-aot--raw-op = 0 0))))
               (funcall thunk)))
           "mrange")))

(ert-deftest nelisp-aot-lisp-arith/mutant-word-test ()
  "Control: a word fixnum test that admits tag 3 (nil, t) is caught."
  (skip-unless (nelisp-aot-lisp-arith-test--home))
  (should (nelisp-aot-lisp-arith-test--mutant-detected
           (lambda (thunk)
             (cl-letf (((symbol-function 'nelisp-aot-compiler--lisp-word-fixnum-form)
                        (lambda (var)
                          `(= (logand (nelisp-aot--word-bits ,var) 1) 1))))
               (funcall thunk)))
           "mword")))

(ert-deftest nelisp-aot-lisp-arith/mutant-incdec ()
  "Control: (1+ X) lowered as an unchecked raw add is caught."
  (skip-unless (nelisp-aot-lisp-arith-test--home))
  (let ((orig-op (symbol-function 'nelisp-aot-compiler--lisp-numeric-op))
        (orig-res (symbol-function 'nelisp-aot-compiler--lisp-fixnum-result-form)))
    (should (nelisp-aot-lisp-arith-test--mutant-detected
             (lambda (thunk)
               (cl-letf (((symbol-function 'nelisp-aot-compiler--lisp-numeric-op)
                          (lambda (op a b env fenv defuns)
                            (if (and (memq op '(+ -))
                                     (eq (nelisp-aot-compiler--ir-kind b) 'imm)
                                     (eql (nelisp-aot-compiler--ir-get b :value) 1))
                                (cl-letf (((symbol-function
                                            'nelisp-aot-compiler--lisp-fixnum-result-form)
                                           (lambda (op2 a2 b2 _fb &optional word)
                                             (if word
                                                 `(nelisp-aot--raw-as-word
                                                   (logior (shl (nelisp-aot--raw-op ,op2 ,a2 ,b2) 2) 1))
                                               `(nelisp-aot--raw-op ,op2 ,a2 ,b2)))))
                                  (funcall orig-op op a b env fenv defuns))
                              (funcall orig-op op a b env fenv defuns)))))
                 (funcall thunk)))
             "minc"))
    (ignore orig-res)))

(ert-deftest nelisp-aot-lisp-arith/diff-home-unusable-is-loud ()
  "A set but unusable NELISP_AOT_DIFF_HOME is an error, an unset one is nil."
  (let ((process-environment (cons "NELISP_AOT_DIFF_HOME=/nonexistent-nelisp-home"
                                   process-environment)))
    (should-error (nelisp-aot-lisp-arith-test--home)))
  (let ((dir (make-temp-file "dh" t)))
    (unwind-protect
        (let ((process-environment (cons (concat "NELISP_AOT_DIFF_HOME=" dir)
                                         process-environment)))
          (should-error (nelisp-aot-lisp-arith-test--home)))
      (delete-directory dir t)))
  (let ((process-environment (cons "NELISP_AOT_DIFF_HOME" process-environment)))
    (should-not (nelisp-aot-lisp-arith-test--home))))

(provide 'nelisp-aot-lisp-arith-test)
;;; nelisp-aot-lisp-arith-test.el ends here
