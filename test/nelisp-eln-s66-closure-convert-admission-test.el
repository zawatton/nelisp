;;; nelisp-eln-s66-closure-convert-admission-test.el --- S6.6 cconv-closure-convert admission -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Focused host tests for admitting the genuine GNU 31.1 (ba35c031)
;; artifact of vendor cconv.el `cconv-closure-convert'
;; (tools/ai/eln-progress.org S6.6):
;;
;;   - its 245-byte body matches exactly the `closure-convert' template:
;;     six authenticated imports (12 `specbind', 945 `Ffuncall' MANY with
;;     argc 3, 4 and 2, 1335 `Fsymbol_value', 1209 `Fnreverse', 10
;;     `set_internal', 4 `helper_unbind_n') and seven d_reloc constants;
;;   - any single changed fixed byte, or a call through an unauthenticated
;;     freloc slot, is rejected;
;;   - its 93-byte registration code is the `gnu-require-subr' skeleton for
;;     a function with `&optional' arguments (arity 1..2, ephemeral relocs
;;     [MIN MAX NAME C-NAME REST]), with type slot 10, lexenv slot 11 and
;;     form slot 9; it is not admitted as a fixed-arity registration and a
;;     fixed-arity registration is not admitted as it; and its type slot
;;     must hold a genuine `(function (t &optional t) VALUE)'.
;;
;; Tests needing the genuine artifact skip when it is absent.  The
;; end-to-end rejections run only when NELISP_S66_BIN names a standalone
;; binary; test/nelisp-eln-s66-nonlocal-smoke.sh covers dynamic-binding
;; restoration on non-local exit.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-eln-registration)
(require 'nelisp-eln-emitter)

(defconst nelisp-eln-s66-test--eln
  (expand-file-name
   "~/.cache/tmp/s6-survey-lex/cconv-closure-convert/overlay/eln/31.1-ba35c031/gnu-cconv-closure-convert.eln")
  "The genuine artifact.")

(defconst nelisp-eln-s66-test--body-vaddr #x1100)
(defconst nelisp-eln-s66-test--top-vaddr #x1200)

(defun nelisp-eln-s66-test--file-bytes ()
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally nelisp-eln-s66-test--eln)
    (buffer-string)))

(defun nelisp-eln-s66-test--body ()
  (substring (nelisp-eln-s66-test--file-bytes)
             nelisp-eln-s66-test--body-vaddr
             (+ nelisp-eln-s66-test--body-vaddr 245)))

(defun nelisp-eln-s66-test--top ()
  (substring (nelisp-eln-s66-test--file-bytes)
             nelisp-eln-s66-test--top-vaddr
             (+ nelisp-eln-s66-test--top-vaddr 93)))

(defun nelisp-eln-s66-test--analyze (bytes)
  (nelisp-eln-tail-code-analyze-multi-import-call
   bytes nelisp-eln-s66-test--body-vaddr))

(ert-deftest nelisp-eln-s66-genuine-body-matches ()
  (skip-unless (file-readable-p nelisp-eln-s66-test--eln))
  (let ((analysis (nelisp-eln-s66-test--analyze (nelisp-eln-s66-test--body))))
    (should (eq (plist-get analysis :shape) 'closure-convert))
    (should (eq (plist-get analysis :proof) :multi-import-call))
    (should (equal (mapcar (lambda (i) (plist-get i :slot))
                           (plist-get analysis :imports))
                   '(12 945 1335 1209 10 4)))
    (should (= (plist-get (car (plist-get analysis :imports)) :got-vaddr)
               #x3fd8))
    (should (equal (mapcar (lambda (d) (plist-get d :slot))
                           (plist-get analysis :data-relocations))
                   '(1 2 3 4 5 7 8)))
    (should (cl-every (lambda (d) (= (plist-get d :got-vaddr) #x3fc8))
                      (plist-get analysis :data-relocations)))
    (should-not (plist-get analysis :module-counter-vaddr))
    (should-not (plist-get analysis :symbols-with-pos-got))))

(ert-deftest nelisp-eln-s66-tampered-body-rejected ()
  (skip-unless (file-readable-p nelisp-eln-s66-test--eln))
  (let* ((bytes (nelisp-eln-s66-test--body))
         (template (plist-get (cdr (assq 'closure-convert
                                         nelisp-eln-tail-code--multi-import-shapes))
                              :template))
         (checked 0))
    (dotimes (i (length template))
      (when (aref template i)
        (let ((tampered (copy-sequence bytes)))
          (aset tampered i (logxor (aref tampered i) 1))
          (should-not (nelisp-eln-s66-test--analyze tampered))
          (setq checked (1+ checked)))))
    (should (= checked (- 245 (* 2 4))))))

(ert-deftest nelisp-eln-s66-unauthenticated-call-target-rejected ()
  (skip-unless (file-readable-p nelisp-eln-s66-test--eln))
  ;; The first `call *0x60(%rbx)' (12 `specbind') -> `*0x68' (13).
  (let ((tampered (copy-sequence (nelisp-eln-s66-test--body))))
    (should (= (aref tampered 41) #x60))
    (aset tampered 41 #x68)
    (should-not (nelisp-eln-s66-test--analyze tampered)))
  (let ((spec (cdr (assq 'closure-convert
                         nelisp-eln-native-subr--multi-import-specs))))
    (should (= (nelisp-eln-native-subr-multi-arity
                (list :shape 'closure-convert))
               2))
    (should (equal (mapcar #'car (plist-get spec :ports))
                   '(12 945 1335 1209 10 4)))
    (dolist (port (plist-get spec :ports))
      (when (eq (nth 1 port) 'fixed)
        (should (plist-get (nelisp-eln-native-subr--multi-port-spec
                            "ba35c031" port)
                           :implementation)))))
  ;; A several-argc MANY port stays only for the variadic `Ffuncall' row,
  ;; with one `lisp' kind per position of its largest argc.
  (dolist (port '((1320 many (2 3 4) (lisp lisp lisp lisp) lisp)
                  (945 many (2 3 4) (lisp lisp lisp) lisp)
                  (12 fixed 1 (lisp) void)
                  (13 fixed 2 (lisp lisp) void)))
    (should-error (nelisp-eln-native-subr--multi-port-spec "ba35c031" port)
                  :type 'nelisp-eln-native-subr-error)))

(ert-deftest nelisp-eln-s66-registration-code-decodes ()
  (skip-unless (file-readable-p nelisp-eln-s66-test--eln))
  (let ((top (nelisp-eln-s66-test--top)))
    (should (nelisp-eln-registration--match-holed-template
             top nelisp-eln-registration--gnu-require-subr-opt-template
             nelisp-eln-registration--gnu-require-subr-holes))
    ;; The fixed-arity skeleton is a different template.
    (should-not (nelisp-eln-registration--match-holed-template
                 top nelisp-eln-registration--gnu-require-subr-template
                 nelisp-eln-registration--gnu-require-subr-holes))
    (should (= (nelisp-eln-registration--gnu-arity top 52) 2))
    (should (= (nelisp-eln-registration--gnu-arity top 61) 1))
    (should (= (nelisp-eln-registration--d-reloc-index top 27) 11))
    (should (= (nelisp-eln-registration--d-reloc-index top 31) 9))
    (should (= (nelisp-eln-registration--d-reloc-index top 50) 10))))

(defun nelisp-eln-s66-test--metadata (type)
  (list :abi-hash "ba35c031"
        :data-relocations
        (vector nil 'cconv-var-classification 'cconv-freevars-alist
                'cconv--dynbound-variables 'cconv-analyze-form
                'cconv-convert 3 'cl--assertion-failed
                '(null cconv-freevars-alist) '(require 'cconv)
                type t 'consp 'listp 'symbol-with-pos-p)
        :ephemeral-data-relocations
        (vector 1 2 'cconv-closure-convert
                (nelisp-eln-emitter--symbol-name 'cconv-closure-convert)
                '(0 nil nil))
        :function-docs ["\n\n(fn FORM &optional DYNBOUND-VARS)"]
        :d-reloc-size 120 :d-reloc-eph-size 40))

(ert-deftest nelisp-eln-s66-type-slot-authenticated ()
  (let* ((extra '(:type-index 10 :min-arity 1 :eph-offset 1))
         (metadata (nelisp-eln-s66-test--metadata
                    '(function (t &optional t) t))))
    (should (= (nelisp-eln-registration--metadata-data-count
                metadata 'gnu-require-subr 2 extra)
               15))
    (should (eq (nelisp-eln-registration--require-effect metadata 9 11)
                'cconv))
    ;; Any other slot does not hold a genuine `(t &optional t)' type.
    (dolist (index '(0 1 6 8 9 11 12))
      (should (eq (cadr (should-error
                         (nelisp-eln-registration--metadata-data-count
                          metadata 'gnu-require-subr 2
                          (list :type-index index :min-arity 1 :eph-offset 1))
                         :type 'nelisp-eln-registration-error))
                  'metadata-outside-emitter-slice)))
    ;; Fixed-arity, `&rest' and wrong-shape types are refused.
    (dolist (bad '((function (t t) t) (function (t &optional t) t t)
                   (function (&optional t t) t) (function (t &rest t) t)
                   (function (t &optional t &optional t) t)
                   (function (t &optional t t) t)))
      (should (eq (cadr (should-error
                         (nelisp-eln-registration--metadata-data-count
                          (nelisp-eln-s66-test--metadata bad)
                          'gnu-require-subr 2 extra)
                         :type 'nelisp-eln-registration-error))
                  'metadata-outside-emitter-slice)))
    ;; The ephemeral vector must carry MIN and MAX arity as GNU writes it.
    (let ((wrong (nelisp-eln-s66-test--metadata '(function (t &optional t) t))))
      (aset (plist-get wrong :ephemeral-data-relocations) 1 3)
      (should (eq (cadr (should-error
                         (nelisp-eln-registration--metadata-data-count
                          wrong 'gnu-require-subr 2 extra)
                         :type 'nelisp-eln-registration-error))
                  'metadata-outside-emitter-slice)))
    ;; `cconv' is an admitted feature, an unrelated one is not.
    (let ((other (nelisp-eln-s66-test--metadata '(function (t &optional t) t))))
      (aset (plist-get other :data-relocations) 9 '(require 'cl-lib))
      (should (eq (cadr (should-error
                         (nelisp-eln-registration--require-effect other 9 11)
                         :type 'nelisp-eln-registration-error))
                  'eval-form-not-admitted)))))

;;; End-to-end on a standalone binary (optional)

(defconst nelisp-eln-s66-test--root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name
                             default-directory))))
  "Repository root, captured while this file loads.")

(defun nelisp-eln-s66-test--run-tampered (offset expected new reason)
  "Run the S6 harness on a copy with file byte OFFSET (EXPECTED) set to NEW
and require a failure naming REASON."
  (let* ((dir (make-temp-file "s66-tamper" t))
         (eln (expand-file-name "gnu-cconv-closure-convert.eln" dir))
         (source (expand-file-name
                  "~/.cache/tmp/s6-survey-lex/cconv-closure-convert/cconv-closure-convert.el")))
    (unwind-protect
        (progn
          (copy-file nelisp-eln-s66-test--eln eln)
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
                    (cons (concat "NELISP_BIN=" (getenv "NELISP_S66_BIN"))
                          process-environment))
                   (default-directory nelisp-eln-s66-test--root)
                   (rc (call-process
                        "sh" nil t nil "test/nelisp-eln-s6-measure.sh"
                        "--eln" eln "--function" "cconv-closure-convert"
                        "--source" source
                        "--corpus"
                        "test/fixtures/s6-corpus/cconv-closure-convert.el"
                        "--allow-stderr")))
              (should-not (eql rc 0))
              (should (string-match-p reason (buffer-string))))))
      (delete-directory dir t))))

(defmacro nelisp-eln-s66-test--e2e (name doc offset expected new reason)
  `(ert-deftest ,name ()
     ,doc
     (skip-unless (and (getenv "NELISP_S66_BIN")
                       (file-executable-p (getenv "NELISP_S66_BIN"))
                       (file-readable-p nelisp-eln-s66-test--eln)))
     (nelisp-eln-s66-test--run-tampered ,offset ,expected ,new ,reason)))

(nelisp-eln-s66-test--e2e
 nelisp-eln-s66-e2e-unauthenticated-call-target-rejected
 "The `Fnreverse' call's slot displacement: 1209 -> 1208 (`Freverse', a
call the genuine host still survives, unlike the `specbind' change the
analysis-level test above makes)."
 (+ nelisp-eln-s66-test--body-vaddr 120) #xc8 #xc0
 "leaf-instructions-not-admitted")

(nelisp-eln-s66-test--e2e
 nelisp-eln-s66-e2e-tampered-body-rejected
 "`test %rax,%rax' after the `Fsymbol_value' call -> `test %rax,%rcx'."
 (+ nelisp-eln-s66-test--body-vaddr 197) #xc0 #xc1
 "leaf-instructions-not-admitted")

(nelisp-eln-s66-test--e2e
 nelisp-eln-s66-e2e-wrong-type-slot-rejected
 "top_level_run's type load `mov 0x50(%rbp),%r8' -> slot 1 (a symbol)."
 (+ nelisp-eln-s66-test--top-vaddr 50) #x50 #x08
 "metadata-outside-emitter-slice")

(provide 'nelisp-eln-s66-closure-convert-admission-test)

;;; nelisp-eln-s66-closure-convert-admission-test.el ends here
