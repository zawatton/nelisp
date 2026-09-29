;;; nelisp-eln-s613-setq-admission-test.el --- S6.13 byte-compile-setq admission -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Focused host tests for admitting the genuine GNU 31.1 (ba35c031)
;; artifact of vendor bytecomp.el `byte-compile-setq'
;; (tools/ai/eln-progress.org S6.13):
;;
;;   - its 369-byte body matches exactly the `setq-form' template: seven
;;     authenticated imports (1250 `Flength', 1320 `Feqlsign' MANY 2, 1220
;;     `Fnth', 945 `Ffuncall' MANY with argc 2 and 3, 1335
;;     `Fsymbol_value', 10 `set_internal', 0 `wrong_type_argument') and
;;     eight d_reloc constants;
;;   - any single changed fixed byte, or a call through an unauthenticated
;;     freloc slot, is rejected; a several-argc MANY port is admitted only
;;     for the variadic `Ffuncall' row;
;;   - its 93-byte registration code is the same `gnu-require-subr' shape
;;     as S6.15's, with type slot 12, lexenv slot 13 and form slot 11, and
;;     a type load pointing anywhere else is refused.
;;
;;   - only this shape declares :OPAQUE-ARGUMENT-SYMBOLS, so only its
;;     native call may carry interned symbols with non-empty state (the
;;     corpus form's `setq') as opaque views; see
;;     test/nelisp-eln-objects-opaque-symbol-smoke.el for the codec side.
;;
;; Tests needing the genuine artifact skip when it is absent.  The
;; end-to-end rejections run only when NELISP_S613_BIN names a standalone
;; binary.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-eln-registration)
(require 'nelisp-eln-emitter)

(defconst nelisp-eln-s613-test--eln
  (expand-file-name
   "~/.cache/tmp/s6-survey-lex/byte-compile-setq/overlay/eln/31.1-ba35c031/gnu-byte-compile-setq.eln")
  "The genuine artifact (sha256 869a0c42..5b96d8d38).")

(defconst nelisp-eln-s613-test--body-vaddr #x1100)
(defconst nelisp-eln-s613-test--top-vaddr #x1280)

(defun nelisp-eln-s613-test--file-bytes ()
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally nelisp-eln-s613-test--eln)
    (buffer-string)))

(defun nelisp-eln-s613-test--body ()
  (substring (nelisp-eln-s613-test--file-bytes)
             nelisp-eln-s613-test--body-vaddr
             (+ nelisp-eln-s613-test--body-vaddr 369)))

(defun nelisp-eln-s613-test--top ()
  (substring (nelisp-eln-s613-test--file-bytes)
             nelisp-eln-s613-test--top-vaddr
             (+ nelisp-eln-s613-test--top-vaddr 93)))

(defun nelisp-eln-s613-test--analyze (bytes)
  (nelisp-eln-tail-code-analyze-multi-import-call
   bytes nelisp-eln-s613-test--body-vaddr))

(ert-deftest nelisp-eln-s613-genuine-body-matches ()
  (skip-unless (file-readable-p nelisp-eln-s613-test--eln))
  (let ((analysis (nelisp-eln-s613-test--analyze (nelisp-eln-s613-test--body))))
    (should (eq (plist-get analysis :shape) 'setq-form))
    (should (equal (mapcar (lambda (i) (plist-get i :slot))
                           (plist-get analysis :imports))
                   '(1250 1320 1220 945 1335 10 0)))
    (should (= (plist-get (car (plist-get analysis :imports)) :got-vaddr)
               #x3fd8))
    (should (equal (mapcar (lambda (d) (plist-get d :slot))
                           (plist-get analysis :data-relocations))
                   '(3 4 5 6 7 9 10 15)))
    (should (cl-every (lambda (d) (= (plist-get d :got-vaddr) #x3fc8))
                      (plist-get analysis :data-relocations)))
    (should-not (plist-get analysis :module-counter-vaddr))
    (should-not (plist-get analysis :symbols-with-pos-got))))

(ert-deftest nelisp-eln-s613-tampered-body-rejected ()
  (skip-unless (file-readable-p nelisp-eln-s613-test--eln))
  (let* ((bytes (nelisp-eln-s613-test--body))
         (template (plist-get (cdr (assq 'setq-form
                                         nelisp-eln-tail-code--multi-import-shapes))
                              :template))
         (checked 0))
    (dotimes (i (length template))
      (when (aref template i)
        (let ((tampered (copy-sequence bytes)))
          (aset tampered i (logxor (aref tampered i) 1))
          (should-not (nelisp-eln-s613-test--analyze tampered))
          (setq checked (1+ checked)))))
    (should (= checked (- 369 (* 2 4))))))

(ert-deftest nelisp-eln-s613-unauthenticated-call-target-rejected ()
  (skip-unless (file-readable-p nelisp-eln-s613-test--eln))
  (let ((tampered (copy-sequence (nelisp-eln-s613-test--body))))
    ;; `call *0x2710(%rbx)' (1250 `Flength') -> `call *0x2718(%rbx)' (1251).
    (should (= (aref tampered 26) #x10))
    (aset tampered 26 #x18)
    (should-not (nelisp-eln-s613-test--analyze tampered)))
  (let ((spec (cdr (assq 'setq-form
                         nelisp-eln-native-subr--multi-import-specs))))
    (should (= (nelisp-eln-native-subr-multi-arity (list :shape 'setq-form))
               1))
    (should (equal (mapcar #'car (plist-get spec :ports))
                   '(1250 1320 1220 945 1335 10 0)))
    (dolist (port (plist-get spec :ports))
      (when (eq (nth 1 port) 'fixed)
        (should (plist-get (nelisp-eln-native-subr--multi-port-spec
                            "ba35c031" port)
                           :implementation)))))
  ;; A several-argc MANY port only for the variadic `Ffuncall' row, with
  ;; one `lisp' kind per position of its largest argc.
  (dolist (port '((1320 many (2 3) (lisp lisp lisp) lisp)
                  (945 many (2 3) (lisp lisp) lisp)
                  (945 many (2 9) (lisp lisp lisp lisp lisp lisp lisp lisp lisp)
                       lisp)
                  (1251 fixed 1 (lisp) lisp)))
    (should-error (nelisp-eln-native-subr--multi-port-spec "ba35c031" port)
                  :type 'nelisp-eln-native-subr-error)))

(ert-deftest nelisp-eln-s613-registration-code-decodes ()
  (skip-unless (file-readable-p nelisp-eln-s613-test--eln))
  (let ((top (nelisp-eln-s613-test--top)))
    (should (nelisp-eln-registration--match-holed-template
             top nelisp-eln-registration--gnu-require-subr-template
             nelisp-eln-registration--gnu-require-subr-holes))
    (should (= (nelisp-eln-registration--gnu-arity top 52) 1))
    (should (= (nelisp-eln-registration--gnu-arity top 61) 1))
    (should (= (nelisp-eln-registration--d-reloc-index top 27) 13))
    (should (= (nelisp-eln-registration--d-reloc-index top 31) 11))
    (should (= (nelisp-eln-registration--d-reloc-index top 50) 12))))

(ert-deftest nelisp-eln-s613-type-slot-authenticated ()
  (let ((metadata
         (list :abi-hash "ba35c031"
               :data-relocations
               (vector 3 nil 2 'byte-compile-form 'byte-compile--for-effect
                       'byte-compile-variable-set 'byte-compile-out 'byte-dup 0
                       'cl--assertion-failed '(= (length form) 3)
                       '(require 'bytecomp) '(function (t) null) t
                       'consp 'listp 'symbol-with-pos-p)
               :ephemeral-data-relocations
               (vector 1 'byte-compile-setq
                       (nelisp-eln-emitter--symbol-name 'byte-compile-setq)
                       '(0 nil nil))
               :function-docs ["\n\n(fn FORM)"]
               :d-reloc-size 136 :d-reloc-eph-size 32)))
    (should (= (nelisp-eln-registration--metadata-data-count
                metadata 'gnu-require-subr 1 '(:type-index 12))
               17))
    (should (eq (nelisp-eln-registration--require-effect metadata 11 13)
                'bytecomp))
    (dolist (index '(0 3 10 11 13))
      (should (eq (cadr (should-error
                         (nelisp-eln-registration--metadata-data-count
                          metadata 'gnu-require-subr 1
                          (list :type-index index))
                         :type 'nelisp-eln-registration-error))
                  'metadata-outside-emitter-slice)))))

;; Only the parse-body, setq-form, if-form, closure-convert shapes and S6.3's
;; accumulate-forms (which likewise only compares, conses and passes symbol
;; words to authenticated ports) may carry opaque interned symbol views.
(ert-deftest nelisp-eln-s613-opaque-symbols-only-for-setq-form ()
  (should (equal (delq nil
                       (mapcar (lambda (entry)
                                 (and (plist-get (cdr entry)
                                                 :opaque-argument-symbols)
                                      (car entry)))
                               nelisp-eln-native-subr--multi-import-specs))
                 ;; S6.12 `if-form' also declares it: it walks FORM's
                 ;; conses and passes symbols only to authenticated ports.
                 ;; S6.3 `accumulate-forms', S6.6 `closure-convert' and S6.4
                 ;; `macroexpand-1' likewise.
                 '(parse-body setq-form if-form accumulate-forms
                   closure-convert macroexpand-1))))

;;; End-to-end on a standalone binary (optional)

(defconst nelisp-eln-s613-test--root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name
                             default-directory))))
  "Repository root, captured while this file loads.")

(defun nelisp-eln-s613-test--run-tampered (offset expected new reason)
  "Run the S6 harness on a copy with file byte OFFSET (EXPECTED) set to
NEW and require a failure naming REASON."
  (let* ((dir (make-temp-file "s613-tamper" t))
         (eln (expand-file-name "gnu-byte-compile-setq.eln" dir))
         (source (expand-file-name
                  "~/.cache/tmp/s6-survey-lex/byte-compile-setq/byte-compile-setq.el")))
    (unwind-protect
        (progn
          (copy-file nelisp-eln-s613-test--eln eln)
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
                    (cons (concat "NELISP_BIN=" (getenv "NELISP_S613_BIN"))
                          process-environment))
                   (default-directory nelisp-eln-s613-test--root)
                   (rc (call-process
                        "sh" nil t nil "test/nelisp-eln-s6-measure.sh"
                        "--eln" eln "--function" "byte-compile-setq"
                        "--source" source
                        "--corpus" "test/fixtures/s6-corpus/byte-compile-setq.el"
                        "--wrapper"
                        "test/fixtures/s6-corpus/byte-compile-setq.wrapper.el"
                        "--allow-stderr")))
              (should-not (eql rc 0))
              (should (string-match-p reason (buffer-string))))))
      (delete-directory dir t))))

(defmacro nelisp-eln-s613-test--e2e (name doc offset expected new reason)
  `(ert-deftest ,name ()
     ,doc
     (skip-unless (and (getenv "NELISP_S613_BIN")
                       (file-executable-p (getenv "NELISP_S613_BIN"))
                       (file-readable-p nelisp-eln-s613-test--eln)))
     (nelisp-eln-s613-test--run-tampered ,offset ,expected ,new ,reason)))

(nelisp-eln-s613-test--e2e
 nelisp-eln-s613-e2e-unauthenticated-call-target-rejected
 "The `Flength' call's slot displacement: 1250 -> 1251."
 (+ nelisp-eln-s613-test--body-vaddr 26) #x10 #x18
 "leaf-instructions-not-admitted")

(nelisp-eln-s613-test--e2e
 nelisp-eln-s613-e2e-tampered-body-rejected
 "`test %rax,%rax' after the `Feqlsign' call -> `test %rax,%rcx'."
 (+ nelisp-eln-s613-test--body-vaddr 66) #xc0 #xc1
 "leaf-instructions-not-admitted")

(nelisp-eln-s613-test--e2e
 nelisp-eln-s613-e2e-wrong-type-slot-rejected
 "top_level_run's type load `mov 0x60(%rbp),%r8' -> slot 1 (nil)."
 (+ nelisp-eln-s613-test--top-vaddr 50) #x60 #x08
 "metadata-outside-emitter-slice")

(provide 'nelisp-eln-s613-setq-admission-test)

;;; nelisp-eln-s613-setq-admission-test.el ends here
