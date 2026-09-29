;;; nelisp-eln-s64-macroexpand-admission-test.el --- S6.4 macroexpand-1 admission -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Focused host tests for admitting the genuine GNU 31.1 (ba35c031)
;; artifact of vendor macroexp.el `macroexpand-1'
;; (tools/ai/eln-progress.org S6.4), the first variable-arity native
;; subr `(FORM &optional ENVIRONMENT)':
;;
;;   - its 462-byte body matches exactly the `macroexpand-1' template: ten
;;     authenticated imports (0 `wrong_type_argument', 7 `slow_eq', 945
;;     `Ffuncall' and 946 `Fapply' MANY 2, 948 `Fautoload_do_load', 1119
;;     `Fcons', 1215 `Fassq', 1339 `Ffboundp', 1350 `Fsymbol_function',
;;     1378 `Fsymbolp'), four d_reloc constants and the
;;     f_symbols_with_pos_enabled_reloc read;
;;   - any single changed fixed byte, or a call through an unauthenticated
;;     freloc slot, is rejected;
;;   - its 69-byte registration code is the `gnu-verified-subr' shape with
;;     MINARGS 1 and MAXARGS 2, its d_reloc_eph words shifted by one (MIN
;;     and MAX both precede the name), and a type slot holding exactly
;;     `(function (t &optional t) t)'; every disagreement between the code,
;;     the type and the metadata is refused;
;;   - end to end, the registered subr reports `func-arity' (1 . 2),
;;     accepts one or two arguments and signals wrong-number-of-arguments
;;     for none or three.
;;
;; Tests needing the genuine artifact skip when it is absent.  The
;; end-to-end checks run only when NELISP_S64_BIN names a standalone binary.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-eln-registration)
(require 'nelisp-eln-emitter)
(require 'nelisp-eln-runtime-services)

(defconst nelisp-eln-s64-test--eln
  (expand-file-name
   "~/.cache/tmp/s6-survey-lex/macroexpand-1/overlay/eln/31.1-ba35c031/gnu-macroexpand-1.eln")
  "The genuine artifact.")

(defconst nelisp-eln-s64-test--body-vaddr #x1100)
(defconst nelisp-eln-s64-test--body-size 462)
(defconst nelisp-eln-s64-test--top-vaddr #x12d0)

(defun nelisp-eln-s64-test--file-bytes ()
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally nelisp-eln-s64-test--eln)
    (buffer-string)))

(defun nelisp-eln-s64-test--body ()
  (substring (nelisp-eln-s64-test--file-bytes)
             nelisp-eln-s64-test--body-vaddr
             (+ nelisp-eln-s64-test--body-vaddr
                nelisp-eln-s64-test--body-size)))

(defun nelisp-eln-s64-test--top ()
  (substring (nelisp-eln-s64-test--file-bytes)
             nelisp-eln-s64-test--top-vaddr
             (+ nelisp-eln-s64-test--top-vaddr 69)))

(defun nelisp-eln-s64-test--analyze (bytes)
  (nelisp-eln-tail-code-analyze-multi-import-call
   bytes nelisp-eln-s64-test--body-vaddr))

(ert-deftest nelisp-eln-s64-genuine-body-matches ()
  (skip-unless (file-readable-p nelisp-eln-s64-test--eln))
  (let ((analysis (nelisp-eln-s64-test--analyze (nelisp-eln-s64-test--body))))
    (should (eq (plist-get analysis :shape) 'macroexpand-1))
    (should (equal (mapcar (lambda (i) (plist-get i :slot))
                           (plist-get analysis :imports))
                   '(0 7 945 946 948 1119 1215 1339 1350 1378)))
    (should (cl-every (lambda (i) (= (plist-get i :got-vaddr) #x3fd8))
                      (plist-get analysis :imports)))
    (should (equal (mapcar (lambda (d) (plist-get d :slot))
                           (plist-get analysis :data-relocations))
                   '(3 5 7 9)))
    (should (cl-every (lambda (d) (= (plist-get d :got-vaddr) #x3fc8))
                      (plist-get analysis :data-relocations)))
    (should (= (plist-get analysis :symbols-with-pos-got) #x3fa8))
    (should-not (plist-get analysis :module-counter-vaddr))))

(ert-deftest nelisp-eln-s64-tampered-body-rejected ()
  (skip-unless (file-readable-p nelisp-eln-s64-test--eln))
  (let* ((bytes (nelisp-eln-s64-test--body))
         (template (plist-get (cdr (assq 'macroexpand-1
                                         nelisp-eln-tail-code--multi-import-shapes))
                              :template))
         (checked 0))
    (dotimes (i (length template))
      (when (aref template i)
        (let ((tampered (copy-sequence bytes)))
          (aset tampered i (logxor (aref tampered i) 1))
          (should-not (nelisp-eln-s64-test--analyze tampered))
          (setq checked (1+ checked)))))
    ;; Three four-byte RIP-relative displacement holes.
    (should (= checked (- nelisp-eln-s64-test--body-size (* 3 4))))))

(ert-deftest nelisp-eln-s64-unauthenticated-call-target-rejected ()
  (skip-unless (file-readable-p nelisp-eln-s64-test--eln))
  (let ((tampered (copy-sequence (nelisp-eln-s64-test--body))))
    ;; `call *0x25f8(%r12)' (1215 `Fassq') -> `call *0x25f0(%r12)' (1214).
    (should (= (aref tampered #x57) #xf8))
    (aset tampered #x57 #xf0)
    (should-not (nelisp-eln-s64-test--analyze tampered)))
  (let ((spec (cdr (assq 'macroexpand-1
                         nelisp-eln-native-subr--multi-import-specs))))
    (should (equal (mapcar #'car (plist-get spec :ports))
                   '(0 7 945 946 948 1119 1215 1339 1350 1378)))
    ;; Fixed ports resolve to their runtime service here; the MANY rows
    ;; (`Ffuncall', `Fapply') bind NeLisp's own `(builtin ...)' cell, which
    ;; only exists on the standalone runtime (see the e2e checks), so only
    ;; their authenticated descriptors are checked on the host.
    (dolist (port (plist-get spec :ports))
      (if (eq (nth 1 port) 'fixed)
          (should (plist-get (nelisp-eln-native-subr--multi-port-spec
                              "ba35c031" port)
                             :implementation))
        (should (equal (nelisp-eln-native-subr--many-descriptor
                        "ba35c031" (car port))
                       (list "ba35c031" (car port)
                             (if (= (car port) 945) 'funcall 'apply) 2))))))
  ;; `Fapply' is a MANY (2, argv) row only; nothing else is authenticated
  ;; under a neighbouring slot or a different convention or arity.
  (dolist (port '((946 many 3 (lisp lisp lisp) lisp)
                  (946 fixed 2 (lisp lisp) lisp)
                  (947 many 2 (lisp lisp) lisp)
                  (948 fixed 2 (lisp lisp) lisp)
                  (1379 fixed 1 (lisp) lisp)
                  (1338 fixed 1 (lisp) lisp)))
    (should (equal (cl-subseq (should-error
                               (nelisp-eln-native-subr--multi-port-spec
                                "ba35c031" port)
                               :type 'nelisp-eln-native-subr-error)
                              0 2)
                   '(nelisp-eln-native-subr-error multi-import-not-admitted)))))

;; Variable-arity shapes: S6.3 `accumulate-forms', S6.6 `closure-convert',
;; S6.4 `macroexpand-1'; only S6.3 and S6.4 read symbols_with_pos_enabled.
(ert-deftest nelisp-eln-s64-only-shape-with-variable-arity-and-swp ()
  (let (optional swp)
    (dolist (entry nelisp-eln-native-subr--multi-import-specs)
      (when (plist-get (cdr entry) :min-arity) (push (car entry) optional))
      (when (plist-get (cdr entry) :symbols-with-pos) (push (car entry) swp)))
    ;; Later lanes added `lambda-form' (S6.9), `cconv-convert-function-form'
    ;; (S6.7) and `compile-form-form' (S10): all `&optional' and all reading
    ;; symbols_with_pos_enabled.
    (should (equal (reverse optional)
                   '(accumulate-forms closure-convert macroexpand-1
                     lambda-form cconv-convert-function-form
                     compile-form-form)))
    (should (equal (reverse swp)
                   '(accumulate-forms macroexpand-1 lambda-form
                     cconv-convert-function-form compile-form-form))))
  (should (= (nelisp-eln-native-subr-multi-arity (list :shape 'macroexpand-1))
             2))
  (should (= (nelisp-eln-native-subr-multi-min-arity
              (list :shape 'macroexpand-1))
             1))
  ;; Every other shape's required count is its own arity.
  (dolist (shape '(set-difference parse-body setq-form cons-form-constant))
    (should (= (nelisp-eln-native-subr-multi-min-arity (list :shape shape))
               (nelisp-eln-native-subr-multi-arity (list :shape shape))))))

(ert-deftest nelisp-eln-s64-runtime-services-authenticated ()
  (dolist (row '((948 "Fautoload_do_load" fixed 3)
                 (1339 "Ffboundp" fixed 1)
                 (1350 "Fsymbol_function" fixed 1)
                 (1378 "Fsymbolp" fixed 1)))
    (let ((d (nelisp-eln-native-subr--runtime-services-descriptor (car row))))
      (should (eq (plist-get d :status) 'supported))
      (should (equal (plist-get d :symbol) (nth 1 row)))
      (should (eq (plist-get d :convention) (nth 2 row)))
      (should (= (plist-get d :arity) (nth 3 row)))))
  (should (eq (nelisp-eln-runtime-services-fsymbolp 'a) t))
  (should-not (nelisp-eln-runtime-services-fsymbolp 5))
  (should (nelisp-eln-runtime-services-ffboundp 'car))
  (should-not (nelisp-eln-runtime-services-ffboundp 'nelisp-s64--no-such-fn))
  (should-error (nelisp-eln-runtime-services-ffboundp 5)
                :type 'wrong-type-argument)
  (should-not (nelisp-eln-runtime-services-fsymbol-function
               'nelisp-s64--no-such-fn))
  (should (equal (nelisp-eln-runtime-services-fautoload-do-load
                  '(a b) 'x 'macro)
                 '(a b))))

(ert-deftest nelisp-eln-s64-registration-code-decodes ()
  (skip-unless (file-readable-p nelisp-eln-s64-test--eln))
  (let ((top (nelisp-eln-s64-test--top)))
    (should (nelisp-eln-registration--match-holed-template
             top nelisp-eln-registration--gnu-verified-subr-template
             nelisp-eln-registration--gnu-verified-subr-holes))
    ;; MAXARGS is loaded into %ecx (offset 45), MINARGS into %edx (54).
    (should (= (nelisp-eln-registration--gnu-arity top 45) 2))
    (should (= (nelisp-eln-registration--gnu-arity top 54) 1))
    (should (= (nelisp-eln-registration--d-reloc-index top 43) 6))
    ;; d_reloc_eph words one slot later than a fixed-arity registration.
    (should (equal (list (aref top 24) (aref top 28) (aref top 32))
                   '(#x20 #x18 #x10)))))

(defun nelisp-eln-s64-test--top-level (bytes)
  "Run the exact `gnu-verified-subr' top-level check on BYTES.
The RIP-relative root-indirection check is stubbed: it is independent of
the arity decisions exercised here."
  (cl-letf (((symbol-function 'nelisp-eln-registration--validate-rip-relocs)
             (lambda (&rest _) t)))
    (nelisp-eln-registration--gnu-verified-subr-top-level
     'handle (list nil nil nil 0) bytes)))

(ert-deftest nelisp-eln-s64-top-level-arity-authenticated ()
  (skip-unless (file-readable-p nelisp-eln-s64-test--eln))
  (let ((top (nelisp-eln-s64-test--top)))
    ;; (MAX TYPE-INDEX MIN)
    (should (equal (nelisp-eln-s64-test--top-level top) '(2 6 1)))
    ;; Each single disagreement is refused, never quietly downgraded:
    ;; 1..1 (MAX 1) with eph words still shifted; 2..2 (MIN 2); 0..2;
    ;; an eph word offset of the fixed layout with a 1..2 registration.
    (dolist (mutation '((45 . #x06) (54 . #x0a) (54 . #x02) (54 . #x0e)
                        (45 . #x0e) (24 . #x18) (28 . #x10) (32 . #x08)))
      (let ((tampered (copy-sequence top)))
        (aset tampered (car mutation) (cdr mutation))
        (should (eq (car (should-error
                          (nelisp-eln-s64-test--top-level tampered)
                          :type 'nelisp-eln-registration-error))
                    'nelisp-eln-registration-error))))
    ;; A changed fixed byte is simply not this shape.
    (let ((tampered (copy-sequence top)))
      (aset tampered 0 #x90)
      (should-not (nelisp-eln-s64-test--top-level tampered)))
    ;; A type slot pointing at the nil slot 0 is refused (index must be
    ;; nonzero); everything else about the slot is checked against the
    ;; artifact's own relocations by the metadata test below.
    (let ((tampered (copy-sequence top)))
      (aset tampered 43 0)
      (should-error (nelisp-eln-s64-test--top-level tampered)
                    :type 'nelisp-eln-registration-error))))

(ert-deftest nelisp-eln-s64-fixed-arity-registration-unchanged ()
  ;; A fixed-arity registration (S6.8's) keeps its own eph layout: the
  ;; three eph offsets 0x18/0x10/0x08 and equal MIN and MAX.
  (let ((fixed (unibyte-string
                #x48 #x83 #xec #x10 #x48 #x8b #x05 #x85 #x2d #x00 #x00 #x48
                #x89 #xfa #x48 #x8b #x0d #x73 #x2d #x00 #x00 #x4c #x8b #x48
                #x18 #x48 #x8b #x70 #x10 #x48 #x8b #x78 #x08 #x48 #x8b #x05
                #x70 #x2d #x00 #x00 #x4c #x8b #x41 #x08 #xb9 #x0a #x00 #x00
                #x00 #x48 #x8b #x00 #x52 #xba #x0a #x00 #x00 #x00 #xff #x90
                #x30 #x20 #x00 #x00 #x48 #x83 #xc4 #x18 #xc3)))
    (should (equal (nelisp-eln-s64-test--top-level fixed) '(2 1 2)))
    ;; The 1..2 eph layout is not accepted with equal MIN/MAX either.
    (let ((tampered (copy-sequence fixed)))
      (aset tampered 24 #x20)
      (should-error (nelisp-eln-s64-test--top-level tampered)
                    :type 'nelisp-eln-registration-error))))

(ert-deftest nelisp-eln-s64-type-slot-authenticated ()
  (should (nelisp-eln-registration--verified-optional-subr-type-p
           '(function (t &optional t) t) 1 2))
  (dolist (type '((function (t t) t)
                  (function (t &optional t &optional t) t)
                  (function (t &rest t) t)
                  (function (&optional t t) t)
                  (function (t) t)
                  (function (t &optional t) t t)
                  nil t fboundp))
    (should-not (nelisp-eln-registration--verified-optional-subr-type-p
                 type 1 2)))
  ;; The fixed-arity meaning is unchanged.
  (should (nelisp-eln-registration--verified-subr-type-p
           '(function (t t) t) 2))
  (should-not (nelisp-eln-registration--verified-subr-type-p
               '(function (t &optional t) t) 2)))

(defun nelisp-eln-s64-test--metadata (&optional type eph0 eph1)
  (list :abi-hash "ba35c031"
        :data-relocations
        (vector nil 'fboundp 'autoload-do-load 'macro 'apply 'macrop
                (or type '(function (t &optional t) t)) t 'consp 'listp
                'symbol-with-pos-p)
        :ephemeral-data-relocations
        (vector (or eph0 1) (or eph1 2) 'macroexpand-1
                (nelisp-eln-emitter--symbol-name 'macroexpand-1)
                '(0 nil nil))
        :function-docs ["\n\n(fn FORM &optional ENVIRONMENT)"]
        :d-reloc-size 88 :d-reloc-eph-size 40))

(ert-deftest nelisp-eln-s64-metadata-arity-authenticated ()
  (should (= (nelisp-eln-registration--metadata-data-count
              (nelisp-eln-s64-test--metadata) 'gnu-verified-subr 2
              '(:type-index 6 :min-arity 1 :eph-offset 1))
             11))
  (dolist (case
           (list
            ;; code says 1..2 but the type slot is a fixed 2-argument type
            (list (nelisp-eln-s64-test--metadata '(function (t t) t))
                  '(:type-index 6 :min-arity 1 :eph-offset 1))
            ;; the registered arity disagrees with the eph MIN/MAX words
            (list (nelisp-eln-s64-test--metadata nil 2 2)
                  '(:type-index 6 :min-arity 1 :eph-offset 1))
            (list (nelisp-eln-s64-test--metadata nil 1 1)
                  '(:type-index 6 :min-arity 1 :eph-offset 1))
            ;; a wrong type slot (index 7 holds t)
            (list (nelisp-eln-s64-test--metadata)
                  '(:type-index 7 :min-arity 1 :eph-offset 1))
            ;; a fixed-arity registration cannot use the 40-byte eph layout
            (list (nelisp-eln-s64-test--metadata)
                  '(:type-index 6))))
    (should (eq (cadr (should-error
                       (nelisp-eln-registration--metadata-data-count
                        (car case) 'gnu-verified-subr 2 (cadr case))
                       :type 'nelisp-eln-registration-error))
                'metadata-outside-emitter-slice))))

;;; End-to-end on a standalone binary (optional)

(defconst nelisp-eln-s64-test--root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name
                             default-directory))))
  "Repository root, captured while this file loads.")

(defun nelisp-eln-s64-test--run-tampered (offset expected new reason)
  "Run the S6 harness on a copy with file byte OFFSET (EXPECTED) set to
NEW and require a failure naming REASON."
  (let* ((dir (make-temp-file "s64-tamper" t))
         (eln (expand-file-name "gnu-macroexpand-1.eln" dir))
         (source (expand-file-name
                  "~/.cache/tmp/s6-survey-lex/macroexpand-1/macroexpand-1.el")))
    (unwind-protect
        (progn
          (copy-file nelisp-eln-s64-test--eln eln)
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert-file-contents-literally eln)
            (should (= (char-after (1+ offset)) expected))
            (goto-char (1+ offset))
            (delete-char 1)
            (insert new)
            (let ((coding-system-for-write 'binary))
              (write-region nil nil eln)))
          (with-temp-buffer
            (let* ((process-environment
                    (cons (concat "NELISP_BIN=" (getenv "NELISP_S64_BIN"))
                          process-environment))
                   (default-directory nelisp-eln-s64-test--root)
                   (rc (call-process
                        "sh" nil t nil "test/nelisp-eln-s6-measure.sh"
                        "--eln" eln "--function" "macroexpand-1"
                        "--source" source
                        "--corpus" "test/fixtures/s6-corpus/macroexpand-1.el")))
              (should-not (eql rc 0))
              (should (string-match-p reason (buffer-string))))))
      (delete-directory dir t))))

(defmacro nelisp-eln-s64-test--e2e (name doc offset expected new reason)
  `(ert-deftest ,name ()
     ,doc
     (skip-unless (and (getenv "NELISP_S64_BIN")
                       (file-executable-p (getenv "NELISP_S64_BIN"))
                       (file-readable-p nelisp-eln-s64-test--eln)))
     (nelisp-eln-s64-test--run-tampered ,offset ,expected ,new ,reason)))

(nelisp-eln-s64-test--e2e
 nelisp-eln-s64-e2e-tampered-body-rejected
 "The displacement byte of the `nopw' padding after the first `ret' (a nop\nstill, so GNU runs the tampered artifact unharmed)."
 (+ nelisp-eln-s64-test--body-vaddr #x2f) #x00 #x01
 "leaf-instructions-not-admitted")

(nelisp-eln-s64-test--e2e
 nelisp-eln-s64-e2e-unauthenticated-call-target-rejected
 "The `Ffboundp' call's slot displacement: 1339 -> 1340 (`Fboundp', also
unary, so GNU survives the tampered artifact)."
 (+ nelisp-eln-s64-test--body-vaddr #xbb) #xd8 #xe0
 "leaf-instructions-not-admitted")

(nelisp-eln-s64-test--e2e
 nelisp-eln-s64-e2e-wrong-type-slot-rejected
 "top_level_run's type load `mov 0x30(%rcx),%r8' -> d_reloc slot 1 (a symbol)."
 (+ nelisp-eln-s64-test--top-vaddr 43) #x30 #x08
 "metadata-outside-emitter-slice")

(nelisp-eln-s64-test--e2e
 nelisp-eln-s64-e2e-arity-mismatch-rejected
 "MINARGS 1 -> 0 while the type slot and metadata still say (t &optional t)."
 (+ nelisp-eln-s64-test--top-vaddr 54) #x06 #x02
 "top-level-instructions-not-admitted")

(ert-deftest nelisp-eln-s64-e2e-call-arity ()
  "The registered subr is (1 . 2): argc 1 and 2 work, 0 and 3 signal."
  (skip-unless (and (getenv "NELISP_S64_BIN")
                    (file-executable-p (getenv "NELISP_S64_BIN"))
                    (file-readable-p nelisp-eln-s64-test--eln)))
  (let ((script (make-temp-file "s64-arity" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file script
            (insert
             (format "%S\n"
                     `(progn
                        (require 'nelisp-eln-registration)
                        (let* ((ns (nelisp-eln-registration-make-isolated-namespace))
                               (nelisp-eln-registration-isolated-namespace ns))
                          (load ,nelisp-eln-s64-test--eln nil t t)
                          (let ((f (nelisp-eln-registration-isolated-function
                                    ns 'macroexpand-1)))
                            (princ (format "S64 subrp=%S arity=%S\n"
                                           (subrp f) (func-arity f)))
                            (dolist (args '((5) ((when x y) nil) nil
                                            ((foo) nil nil)))
                              (princ (format "S64 %S => %S\n" args
                                             (condition-case e (apply f args)
                                               (error e)))))))))))
          (with-temp-buffer
            (let ((default-directory temporary-file-directory))
              (call-process (getenv "NELISP_S64_BIN") nil t nil
                            "--load" script "--"))
            (let ((out (buffer-string)))
              (should (string-match-p "S64 subrp=t arity=(1 \\. 2)" out))
              (should (string-match-p "S64 (5) => 5" out))
              (should (string-match-p
                       "S64 ((when x y) nil) => (if x (progn y))" out))
              (should (string-match-p
                       "S64 nil => (wrong-number-of-arguments macroexpand-1 0)"
                       out))
              (should (string-match-p
                       "wrong-number-of-arguments macroexpand-1 3)" out)))))
      (delete-file script))))

(provide 'nelisp-eln-s64-macroexpand-admission-test)

;;; nelisp-eln-s64-macroexpand-admission-test.el ends here
