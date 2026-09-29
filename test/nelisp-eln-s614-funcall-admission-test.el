;;; nelisp-eln-s614-funcall-admission-test.el --- S6.14 byte-compile-funcall admission -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Focused host tests for admitting the genuine GNU 31.1 (ba35c031)
;; artifact of vendor bytecomp.el `byte-compile-funcall'
;; (tools/ai/eln-progress.org S6.14):
;;
;;   - its 263-byte body matches exactly the `funcall-form' template: six
;;     authenticated imports (1194 `Fmapc', 1250 `Flength', 945 `Ffuncall'
;;     MANY with argc 2 and 3, 0 `wrong_type_argument', 703
;;     `Fformat_message' MANY 1, 1335 `Fsymbol_value') and eight d_reloc
;;     constants;
;;   - any single changed fixed byte, or a call through an unauthenticated
;;     freloc slot, is rejected; a `Fformat_message' port is admitted only
;;     with exactly one argument;
;;   - its 93-byte registration code is the same `gnu-require-subr' shape
;;     as S6.13's, with type slot 11, lexenv slot 12 and form slot 10, and
;;     a type load pointing anywhere else, or arities that disagree with
;;     the body, are refused;
;;   - only shapes that declare :OPAQUE-ARGUMENT-SYMBOLS may carry interned
;;     symbols with non-empty state (the corpus form's `funcall') as
;;     opaque views.
;;
;; Tests needing the genuine artifact skip when it is absent.  The
;; end-to-end rejections run only when NELISP_S614_BIN names a standalone
;; binary.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-eln-registration)
(require 'nelisp-eln-emitter)
(require 'nelisp-eln-runtime-services)

(defconst nelisp-eln-s614-test--eln
  (expand-file-name
   "~/.cache/tmp/s6-survey-lex/byte-compile-funcall/overlay/eln/31.1-ba35c031/gnu-byte-compile-funcall.eln")
  "The genuine artifact.")

(defconst nelisp-eln-s614-test--body-vaddr #x1100)
(defconst nelisp-eln-s614-test--body-size 263)
(defconst nelisp-eln-s614-test--top-vaddr #x1210)

(defun nelisp-eln-s614-test--file-bytes ()
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally nelisp-eln-s614-test--eln)
    (buffer-string)))

(defun nelisp-eln-s614-test--body ()
  (substring (nelisp-eln-s614-test--file-bytes)
             nelisp-eln-s614-test--body-vaddr
             (+ nelisp-eln-s614-test--body-vaddr
                nelisp-eln-s614-test--body-size)))

(defun nelisp-eln-s614-test--top ()
  (substring (nelisp-eln-s614-test--file-bytes)
             nelisp-eln-s614-test--top-vaddr
             (+ nelisp-eln-s614-test--top-vaddr 93)))

(defun nelisp-eln-s614-test--analyze (bytes)
  (nelisp-eln-tail-code-analyze-multi-import-call
   bytes nelisp-eln-s614-test--body-vaddr))

(ert-deftest nelisp-eln-s614-genuine-body-matches ()
  (skip-unless (file-readable-p nelisp-eln-s614-test--eln))
  (let ((analysis (nelisp-eln-s614-test--analyze (nelisp-eln-s614-test--body))))
    (should (eq (plist-get analysis :shape) 'funcall-form))
    (should (equal (mapcar (lambda (i) (plist-get i :slot))
                           (plist-get analysis :imports))
                   '(1194 1250 945 0 703 1335)))
    (should (= (plist-get (car (plist-get analysis :imports)) :got-vaddr)
               #x3fd8))
    (should (equal (mapcar (lambda (d) (plist-get d :slot))
                           (plist-get analysis :data-relocations))
                   '(1 3 4 5 6 8 9 14)))
    (should (cl-every (lambda (d) (= (plist-get d :got-vaddr) #x3fc8))
                      (plist-get analysis :data-relocations)))
    (should-not (plist-get analysis :module-counter-vaddr))
    (should-not (plist-get analysis :symbols-with-pos-got))))

(ert-deftest nelisp-eln-s614-tampered-body-rejected ()
  (skip-unless (file-readable-p nelisp-eln-s614-test--eln))
  (let* ((bytes (nelisp-eln-s614-test--body))
         (template (plist-get (cdr (assq 'funcall-form
                                         nelisp-eln-tail-code--multi-import-shapes))
                              :template))
         (checked 0))
    (should (= (length template) nelisp-eln-s614-test--body-size))
    (dotimes (i (length template))
      (when (aref template i)
        (let ((tampered (copy-sequence bytes)))
          (aset tampered i (logxor (aref tampered i) 1))
          (should-not (nelisp-eln-s614-test--analyze tampered))
          (setq checked (1+ checked)))))
    ;; Three four-byte RIP-relative loads are the only holes.
    (should (= checked (- nelisp-eln-s614-test--body-size (* 3 4))))
    ;; A truncated or extended body never matches either.
    (should-not (nelisp-eln-s614-test--analyze (substring bytes 0 262)))
    (should-not (nelisp-eln-s614-test--analyze (concat bytes "\x90")))))

(ert-deftest nelisp-eln-s614-rip-loads-must-agree ()
  "Loads of one GOT kind must all reach the same slot."
  (skip-unless (file-readable-p nelisp-eln-s614-test--eln))
  (let ((body (nelisp-eln-s614-test--body)))
    (should (nelisp-eln-s614-test--analyze body))
    ;; Moving only the second `d_reloc' load (disp32 at 131) away from the
    ;; first one (at 39) is rejected, and so is moving only the first one.
    (dolist (offset '(131 39))
      (let ((tampered (copy-sequence body)))
        (aset tampered offset (logxor (aref tampered offset) 8))
        (should-not (nelisp-eln-s614-test--analyze tampered))))))

(ert-deftest nelisp-eln-s614-unauthenticated-call-target-rejected ()
  (skip-unless (file-readable-p nelisp-eln-s614-test--eln))
  (let ((tampered (copy-sequence (nelisp-eln-s614-test--body))))
    ;; `call *0x2550(%rbp)' (1194 `Fmapc') -> `call *0x2558(%rbp)' (1195).
    (should (= (aref tampered 55) #x50))
    (aset tampered 55 #x58)
    (should-not (nelisp-eln-s614-test--analyze tampered)))
  (let ((spec (cdr (assq 'funcall-form
                         nelisp-eln-native-subr--multi-import-specs))))
    (should (= (nelisp-eln-native-subr-multi-arity (list :shape 'funcall-form))
               1))
    (should (equal (mapcar #'car (plist-get spec :ports))
                   '(1194 1250 945 0 703 1335)))
    (should (plist-get spec :opaque-argument-symbols))
    (should-not (plist-get spec :opaque-vectors))
    ;; The fixed ports and the `Fformat_message' service resolve to their
    ;; runtime-services implementation (the variadic `Ffuncall' row is a
    ;; canonical builtin, resolved at call time).
    (dolist (port (plist-get spec :ports))
      (unless (eq (car port) 945)
        (should (plist-get (nelisp-eln-native-subr--multi-port-spec
                            "ba35c031" port)
                           :implementation)))))
  ;; `Fformat_message' is admitted only as MANY with exactly one argument;
  ;; `Fmapc' only as the fixed two-argument row; a several-argc MANY port
  ;; only for the variadic `Ffuncall' row.
  (dolist (port '((703 many 2 (lisp lisp) lisp)
                  (703 many (1 2) (lisp lisp) lisp)
                  (703 fixed 1 (lisp) lisp)
                  (1194 fixed 1 (lisp) lisp)
                  (1194 many 2 (lisp lisp) lisp)
                  (1195 many 1 (lisp) lisp)
                  (1193 fixed 2 (lisp lisp) lisp)
                  (702 many 1 (lisp) lisp)
                  (945 many (2 3) (lisp lisp) lisp)))
    (should-error (nelisp-eln-native-subr--multi-port-spec "ba35c031" port)
                  :type 'nelisp-eln-native-subr-error))
  ;; The service slots stay unavailable under another ABI hash.
  (should-error (nelisp-eln-native-subr--multi-port-spec
                 "00000000" '(703 many 1 (lisp) lisp))
                :type 'nelisp-eln-native-subr-error))

(ert-deftest nelisp-eln-s614-constants-authenticated ()
  (let ((spec (cdr (assq 'funcall-form
                         nelisp-eln-native-subr--multi-import-specs))))
    (should (equal (mapcar #'car (plist-get spec :constants))
                   '(1 3 4 5 6 8 9 14)))
    ;; Each expected constant matches itself and nothing near it.
    (dolist (c (plist-get spec :constants))
      (should (nelisp-eln-native-subr--multi-constant-matches-p
               (cdr c) (cdr c))))
    (should-not (nelisp-eln-native-subr--multi-constant-matches-p
                 'byte-compile-out (cdr (assq 4 (plist-get spec :constants)))))
    (should-not (nelisp-eln-native-subr--multi-constant-matches-p
                 "`funcall' called with no argument"
                 (cdr (assq 3 (plist-get spec :constants)))))
    (should-not (nelisp-eln-native-subr--multi-constant-matches-p
                 '(signal 'wrong-number-of-arguments '(funcall 1))
                 (cdr (assq 5 (plist-get spec :constants)))))))

(ert-deftest nelisp-eln-s614-runtime-services-match-builtins ()
  (let* ((seen nil)
         (list (list 1 2 3))
         (result (nelisp-eln-runtime-services-fmapc
                  (lambda (x) (push x seen)) list)))
    (should (eq result list))
    (should (equal (nreverse seen) '(1 2 3))))
  (should (equal (nelisp-eln-runtime-services-fformat-message "a%sb" 1)
                 "a1b"))
  (should (stringp (nelisp-eln-runtime-services-fformat-message
                    "`funcall' called with no arguments")))
  (dolist (row '((1194 "Fmapc" fixed 2) (703 "Fformat_message" many 1)))
    (let ((d (cl-find-if (lambda (x) (eql (plist-get x :index) (car row)))
                         nelisp-eln-runtime-services-descriptors)))
      (should (equal (plist-get d :symbol) (nth 1 row)))
      (should (eq (plist-get d :convention) (nth 2 row)))
      (should (equal (plist-get d :arity) (nth 3 row)))
      (should (eq (plist-get d :status) 'supported)))))

(ert-deftest nelisp-eln-s614-registration-code-decodes ()
  (skip-unless (file-readable-p nelisp-eln-s614-test--eln))
  (let ((top (nelisp-eln-s614-test--top)))
    (should (nelisp-eln-registration--match-holed-template
             top nelisp-eln-registration--gnu-require-subr-template
             nelisp-eln-registration--gnu-require-subr-holes))
    (should (= (nelisp-eln-registration--gnu-arity top 52) 1))
    (should (= (nelisp-eln-registration--gnu-arity top 61) 1))
    (should (= (nelisp-eln-registration--d-reloc-index top 27) 12))
    (should (= (nelisp-eln-registration--d-reloc-index top 31) 10))
    (should (= (nelisp-eln-registration--d-reloc-index top 50) 11))))

(ert-deftest nelisp-eln-s614-type-slot-authenticated ()
  (let ((metadata
         (list :abi-hash "ba35c031"
               :data-relocations
               (vector nil 'byte-compile-report-error 'format-message
                       "`funcall' called with no arguments"
                       'byte-compile-form
                       '(signal 'wrong-number-of-arguments '(funcall 0))
                       'byte-compile--for-effect 'mapc 'byte-compile-out
                       'byte-call '(require 'bytecomp) '(function (t) t) t
                       'consp 'listp 'symbol-with-pos-p)
               :ephemeral-data-relocations
               (vector 1 'byte-compile-funcall
                       (nelisp-eln-emitter--symbol-name 'byte-compile-funcall)
                       '(0 nil nil))
               :function-docs ["\n\n(fn FORM)"]
               :d-reloc-size 128 :d-reloc-eph-size 32)))
    (should (= (nelisp-eln-registration--metadata-data-count
                metadata 'gnu-require-subr 1 '(:type-index 11))
               16))
    (should (eq (nelisp-eln-registration--require-effect metadata 10 12)
                'bytecomp))
    (dolist (index '(0 1 3 10 12))
      (should (eq (cadr (should-error
                         (nelisp-eln-registration--metadata-data-count
                          metadata 'gnu-require-subr 1
                          (list :type-index index))
                         :type 'nelisp-eln-registration-error))
                  'metadata-outside-emitter-slice)))))

(ert-deftest nelisp-eln-s614-opaque-symbols-scoped-to-declaring-shapes ()
  "Only shapes that declare :OPAQUE-ARGUMENT-SYMBOLS carry opaque symbols."
  (let ((declared (delq nil
                        (mapcar (lambda (entry)
                                  (and (plist-get (cdr entry)
                                                  :opaque-argument-symbols)
                                       (car entry)))
                                nelisp-eln-native-subr--multi-import-specs))))
    (should (memq 'funcall-form declared))
    ;; The port kinds `funcall-form' adds (`handle-nil' results of `Fmapc'
    ;; and `Fformat_message') are used by no shape that predates it.
    (dolist (entry nelisp-eln-native-subr--multi-import-specs)
      (unless (eq (car entry) 'funcall-form)
        (dolist (port (plist-get (cdr entry) :ports))
          (should-not (memq (nth 0 port) '(1194 703))))))))

;;; End-to-end on a standalone binary (optional)

(defconst nelisp-eln-s614-test--root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name
                             default-directory))))
  "Repository root, captured while this file loads.")

(defun nelisp-eln-s614-test--run-tampered (edits reason)
  "Run the S6 harness on a copy with EDITS ((OFFSET EXPECTED NEW) ...)
applied and require a failure naming REASON."
  (let* ((dir (make-temp-file "s614-tamper" t))
         (eln (expand-file-name "gnu-byte-compile-funcall.eln" dir))
         (source (expand-file-name
                  "~/.cache/tmp/s6-survey-lex/byte-compile-funcall/byte-compile-funcall.el")))
    (unwind-protect
        (progn
          (copy-file nelisp-eln-s614-test--eln eln)
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert-file-contents-literally eln)
            (dolist (edit edits)
              (should (= (char-after (1+ (nth 0 edit))) (nth 1 edit)))
              (goto-char (1+ (nth 0 edit)))
              (delete-char 1)
              (insert (nth 2 edit)))
            (let ((coding-system-for-write 'binary))
              (write-region nil nil eln)))
          (with-temp-buffer
            (let* ((process-environment
                    (cons (concat "NELISP_BIN=" (getenv "NELISP_S614_BIN"))
                          process-environment))
                   (default-directory nelisp-eln-s614-test--root)
                   (rc (call-process
                        "sh" nil t nil "test/nelisp-eln-s6-measure.sh"
                        "--eln" eln "--function" "byte-compile-funcall"
                        "--source" source
                        "--corpus" "test/fixtures/s6-corpus/byte-compile-funcall.el"
                        "--wrapper"
                        "test/fixtures/s6-corpus/byte-compile-funcall.wrapper.el"
                        "--allow-stderr")))
              (should-not (eql rc 0))
              (should (string-match-p reason (buffer-string))))))
      (delete-directory dir t))))

(defmacro nelisp-eln-s614-test--e2e (name doc edits reason)
  `(ert-deftest ,name ()
     ,doc
     (skip-unless (and (getenv "NELISP_S614_BIN")
                       (file-executable-p (getenv "NELISP_S614_BIN"))
                       (file-readable-p nelisp-eln-s614-test--eln)))
     (nelisp-eln-s614-test--run-tampered ,edits ,reason)))

(nelisp-eln-s614-test--e2e
 nelisp-eln-s614-e2e-unauthenticated-call-target-rejected
 "The `Fmapc' call's slot displacement: 1194 -> 1195 (`Fmapcar')."
 (list (list (+ nelisp-eln-s614-test--body-vaddr 55) #x50 #x58))
 "leaf-instructions-not-admitted")

(nelisp-eln-s614-test--e2e
 nelisp-eln-s614-e2e-tampered-body-rejected
 "`test %rsi,%rsi' after the first tag check -> `test %rsi,%rdi'."
 (list (list (+ nelisp-eln-s614-test--body-vaddr 45) #xf6 #xf7))
 "leaf-instructions-not-admitted")

(nelisp-eln-s614-test--e2e
 nelisp-eln-s614-e2e-tampered-format-message-slot-rejected
 "The `Fformat_message' call's slot displacement: 703 -> 704 (`Fformat')."
 ;; `call *0x15f8(%rbp)' -> `call *0x1600(%rbp)': the low displacement byte.
 (list (list (+ nelisp-eln-s614-test--body-vaddr 173) #xf8 #x00))
 "leaf-instructions-not-admitted")

(nelisp-eln-s614-test--e2e
 nelisp-eln-s614-e2e-wrong-arity-rejected
 "Both registration arity immediates 1 -> 2 no longer fit the type slot's
`(function (t) t)' (and the one-argument body)."
 (list (list (+ nelisp-eln-s614-test--top-vaddr 52) #x06 #x0a)
       (list (+ nelisp-eln-s614-test--top-vaddr 61) #x06 #x0a))
 "metadata-outside-emitter-slice")

(nelisp-eln-s614-test--e2e
 nelisp-eln-s614-e2e-min-max-arity-mismatch-rejected
 "Registration min arity 2 with max arity 1 is refused."
 (list (list (+ nelisp-eln-s614-test--top-vaddr 61) #x06 #x0a))
 "top-level-instructions-not-admitted")

(nelisp-eln-s614-test--e2e
 nelisp-eln-s614-e2e-wrong-type-slot-rejected
 "top_level_run's type load `mov 0x58(%rbp),%r8' -> slot 1 (a symbol)."
 (list (list (+ nelisp-eln-s614-test--top-vaddr 50) #x58 #x08))
 "metadata-outside-emitter-slice")

(provide 'nelisp-eln-s614-funcall-admission-test)

;;; nelisp-eln-s614-funcall-admission-test.el ends here
