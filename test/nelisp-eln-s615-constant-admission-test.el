;;; nelisp-eln-s615-constant-admission-test.el --- S6.15 byte-compile-constant admission -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Focused host tests for admitting the genuine GNU 31.1 (ba35c031)
;; artifact of vendor bytecomp.el `byte-compile-constant'
;; (tools/ai/eln-progress.org S6.15):
;;
;;   - its 110-byte body matches exactly the `for-effect-constant'
;;     template: three authenticated imports (1335 `Fsymbol_value', 945
;;     `Ffuncall' MANY 2, 10 `set_internal'), d_reloc[0]
;;     `byte-compile--for-effect' and d_reloc[2]
;;     `byte-compile-push-constant';
;;   - any single changed fixed byte, or a call through an unauthenticated
;;     freloc slot, is rejected;
;;   - the 93-byte `gnu-require-subr' registration code decodes arity 1,
;;     type slot 4, Feval lexenv slot 5 and form slot 3; the form must
;;     decode to an admitted `(require \\='FEATURE)', and a registration
;;     pointing its type load at a slot that does not hold a genuine
;;     fixed-arity-1 function type is refused.
;;
;; Tests needing the genuine artifact skip when it is absent.  The
;; end-to-end rejections run only when NELISP_S615_BIN names a standalone
;; binary.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-eln-registration)
(require 'nelisp-eln-emitter)

(defconst nelisp-eln-s615-test--eln
  (expand-file-name
   "~/.cache/tmp/s6-survey-lex/byte-compile-constant/overlay/eln/31.1-ba35c031/gnu-byte-compile-constant.eln")
  "The genuine artifact (sha256 f837d057..86f5de).")

(defconst nelisp-eln-s615-test--body-vaddr #x1100)
(defconst nelisp-eln-s615-test--top-vaddr #x1170)

(defun nelisp-eln-s615-test--file-bytes ()
  "Return the genuine artifact's bytes as a unibyte string."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally nelisp-eln-s615-test--eln)
    (buffer-string)))

(defun nelisp-eln-s615-test--body ()
  "Return the genuine body (text is mapped at file offset = vaddr)."
  (substring (nelisp-eln-s615-test--file-bytes)
             nelisp-eln-s615-test--body-vaddr
             (+ nelisp-eln-s615-test--body-vaddr 110)))

(defun nelisp-eln-s615-test--top ()
  (substring (nelisp-eln-s615-test--file-bytes)
             nelisp-eln-s615-test--top-vaddr
             (+ nelisp-eln-s615-test--top-vaddr 93)))

(defun nelisp-eln-s615-test--analyze (bytes)
  (nelisp-eln-tail-code-analyze-multi-import-call
   bytes nelisp-eln-s615-test--body-vaddr))

;;; Body template

(ert-deftest nelisp-eln-s615-genuine-body-matches ()
  (skip-unless (file-readable-p nelisp-eln-s615-test--eln))
  (let ((analysis (nelisp-eln-s615-test--analyze (nelisp-eln-s615-test--body))))
    (should (eq (plist-get analysis :shape) 'for-effect-constant))
    (should (eq (plist-get analysis :proof) :multi-import-call))
    (should (equal (mapcar (lambda (i) (plist-get i :slot))
                           (plist-get analysis :imports))
                   '(1335 945 10)))
    (should (= (plist-get (car (plist-get analysis :imports)) :got-vaddr)
               #x3fd8))
    (should (equal (mapcar (lambda (d) (list (plist-get d :slot)
                                             (plist-get d :got-vaddr)))
                           (plist-get analysis :data-relocations))
                   '((0 #x3fc8) (2 #x3fc8))))
    (should-not (plist-get analysis :module-counter-vaddr))
    (should-not (plist-get analysis :symbols-with-pos-got))))

(ert-deftest nelisp-eln-s615-tampered-body-rejected ()
  "Every single changed fixed byte of the body matches no shape."
  (skip-unless (file-readable-p nelisp-eln-s615-test--eln))
  (let* ((bytes (nelisp-eln-s615-test--body))
         (template (plist-get (cdr (assq 'for-effect-constant
                                         nelisp-eln-tail-code--multi-import-shapes))
                              :template))
         (checked 0))
    (dotimes (i (length template))
      (when (aref template i)
        (let ((tampered (copy-sequence bytes)))
          (aset tampered i (logxor (aref tampered i) 1))
          (should-not (nelisp-eln-s615-test--analyze tampered))
          (setq checked (1+ checked)))))
    ;; 110 bytes minus two GOT displacements.
    (should (= checked (- 110 (* 2 4))))
    (should-not (nelisp-eln-s615-test--analyze (substring bytes 0 109)))))

(ert-deftest nelisp-eln-s615-unauthenticated-call-target-rejected ()
  "Calling a freloc slot other than the authenticated three is refused."
  (skip-unless (file-readable-p nelisp-eln-s615-test--eln))
  (let ((tampered (copy-sequence (nelisp-eln-s615-test--body))))
    ;; `call *0x29b8(%rbx)' (1335 `Fsymbol_value') -> `call *0x29b0(%rbx)'
    ;; (1334, which has no NeLisp runtime-services implementation).
    (should (= (aref tampered 34) #xb8))
    (aset tampered 34 #xb0)
    (should-not (nelisp-eln-s615-test--analyze tampered)))
  ;; Every fixed port of the admitted spec authenticates (the MANY slot
  ;; 945 is authenticated against the runtime's own canonical `funcall',
  ;; which only the standalone runtime has; the e2e run covers it) ...
  (let ((spec (cdr (assq 'for-effect-constant
                         nelisp-eln-native-subr--multi-import-specs))))
    (should (= (nelisp-eln-native-subr-multi-arity
                (list :shape 'for-effect-constant))
               1))
    (should (equal (plist-get spec :constants)
                   '((0 . byte-compile--for-effect)
                     (2 . byte-compile-push-constant))))
    (dolist (port (plist-get spec :ports))
      (when (eq (nth 1 port) 'fixed)
       (should (plist-get (nelisp-eln-native-subr--multi-port-spec
                          "ba35c031" port)
                         :implementation)))))
  ;; ... and an unimplemented slot, or a known slot at the wrong
  ;; convention/arity, does not.
  (should (equal (cdr (should-error
                        (nelisp-eln-native-subr--multi-port-spec
                         "ba35c031" '(1334 fixed 1 (lisp) lisp))
                        :type 'nelisp-eln-native-subr-error))
                 '(multi-import-not-admitted unauthenticated-fixed-slot 1334)))
  (should-error (nelisp-eln-native-subr--multi-port-spec
                 "ba35c031" '(10 fixed 3 (lisp lisp lisp) void))
                :type 'nelisp-eln-native-subr-error))

;;; Registration code

(defun nelisp-eln-s615-test--metadata (&optional eph0 form)
  (list :abi-hash "ba35c031"
        :data-relocations
        (vector 'byte-compile--for-effect nil 'byte-compile-push-constant
                (or form '(require 'bytecomp)) '(function (t) t) t
                'consp 'listp 'symbol-with-pos-p)
        :ephemeral-data-relocations
        (vector (or eph0 1) 'byte-compile-constant
                (nelisp-eln-emitter--symbol-name 'byte-compile-constant)
                '(0 nil nil))
        :function-docs ["\n\n(fn CONST)"]
        :d-reloc-size 72 :d-reloc-eph-size 32))

(ert-deftest nelisp-eln-s615-registration-code-decodes ()
  (skip-unless (file-readable-p nelisp-eln-s615-test--eln))
  (let ((top (nelisp-eln-s615-test--top)))
    (should (nelisp-eln-registration--match-holed-template
             top nelisp-eln-registration--gnu-require-subr-template
             nelisp-eln-registration--gnu-require-subr-holes))
    (should (= (nelisp-eln-registration--gnu-arity top 52) 1))
    (should (= (nelisp-eln-registration--gnu-arity top 61) 1))
    (should (= (nelisp-eln-registration--d-reloc-index top 27) 5))
    (should (= (nelisp-eln-registration--d-reloc-index top 31) 3))
    (should (= (nelisp-eln-registration--d-reloc-index top 50) 4))
    ;; No other registration template matches it.
    (should-not (nelisp-eln-registration--match-holed-template
                 top nelisp-eln-registration--gnu-verified-subr-template
                 nelisp-eln-registration--gnu-verified-subr-holes))
    ;; Any changed fixed byte is no shape.
    (dotimes (i (length top))
      (unless (nelisp-eln-registration--offset-holed-p
               i nelisp-eln-registration--gnu-require-subr-holes)
        (let ((tampered (copy-sequence top)))
          (aset tampered i (logxor (aref tampered i) 1))
          (should-not (nelisp-eln-registration--match-holed-template
                       tampered nelisp-eln-registration--gnu-require-subr-template
                       nelisp-eln-registration--gnu-require-subr-holes)))))))

(ert-deftest nelisp-eln-s615-type-slot-authenticated ()
  (let ((metadata (nelisp-eln-s615-test--metadata)))
    (should (= (nelisp-eln-registration--metadata-data-count
                metadata 'gnu-require-subr 1 '(:type-index 4))
               9))
    ;; Wrong type slot: a symbol, nil, the require form, t, out of range.
    (dolist (index '(0 1 3 5 9))
      (should (eq (cadr (should-error
                         (nelisp-eln-registration--metadata-data-count
                          metadata 'gnu-require-subr 1
                          (list :type-index index))
                         :type 'nelisp-eln-registration-error))
                  'metadata-outside-emitter-slice)))
    ;; A type whose argument count disagrees with the registered arity.
    (should-error (nelisp-eln-registration--metadata-data-count
                   metadata 'gnu-require-subr 2 '(:type-index 4))
                  :type 'nelisp-eln-registration-error)
    ;; The top-level-unused eph word must be the genuine arity-1 value.
    (should-error (nelisp-eln-registration--metadata-data-count
                   (nelisp-eln-s615-test--metadata 2)
                   'gnu-require-subr 1 '(:type-index 4))
                  :type 'nelisp-eln-registration-error)))

(ert-deftest nelisp-eln-s615-require-effect-authenticated ()
  (should (eq (nelisp-eln-registration--require-effect
               (nelisp-eln-s615-test--metadata) 3 5)
              'bytecomp))
  (dolist (case (list
                 ;; Lexenv slot that is not `t'.
                 (list (nelisp-eln-s615-test--metadata) 3 1)
                 ;; Form slot that is not the require form.
                 (list (nelisp-eln-s615-test--metadata) 4 5)
                 (list (nelisp-eln-s615-test--metadata) 9 5)
                 ;; A feature outside the admitted list.
                 (list (nelisp-eln-s615-test--metadata nil '(require 'shell))
                       3 5)
                 ;; Any other form, even one naming the admitted feature.
                 (list (nelisp-eln-s615-test--metadata
                        nil '(require 'bytecomp nil t))
                       3 5)
                 (list (nelisp-eln-s615-test--metadata nil '(load "bytecomp"))
                       3 5)
                 (list (nelisp-eln-s615-test--metadata nil '(require bytecomp))
                       3 5)))
    (should (eq (cadr (should-error
                       (apply #'nelisp-eln-registration--require-effect case)
                       :type 'nelisp-eln-registration-error))
                'eval-form-not-admitted))))

;;; End-to-end on a standalone binary (optional)

(defconst nelisp-eln-s615-test--root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name
                             default-directory))))
  "Repository root, captured while this file loads.")

(defun nelisp-eln-s615-test--run-tampered (offset expected new reason)
  "Run the S6 harness on a copy with file byte OFFSET (EXPECTED) set to
NEW and require a failure naming REASON."
  (let* ((dir (make-temp-file "s615-tamper" t))
         (eln (expand-file-name "gnu-byte-compile-constant.eln" dir))
         (source (expand-file-name
                  "~/.cache/tmp/s6-survey-lex/byte-compile-constant/byte-compile-constant.el")))
    (unwind-protect
        (progn
          (copy-file nelisp-eln-s615-test--eln eln)
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
                    (cons (concat "NELISP_BIN=" (getenv "NELISP_S615_BIN"))
                          process-environment))
                   (default-directory nelisp-eln-s615-test--root)
                   (rc (call-process
                        "sh" nil t nil "test/nelisp-eln-s6-measure.sh"
                        "--eln" eln "--function" "byte-compile-constant"
                        "--source" source
                        "--corpus" "test/fixtures/s6-corpus/byte-compile-constant.el"
                        "--wrapper"
                        "test/fixtures/s6-corpus/byte-compile-constant.wrapper.el")))
              (should-not (eql rc 0))
              (should (string-match-p reason (buffer-string))))))
      (delete-directory dir t))))

(defmacro nelisp-eln-s615-test--e2e (name doc offset expected new reason)
  `(ert-deftest ,name ()
     ,doc
     (skip-unless (and (getenv "NELISP_S615_BIN")
                       (file-executable-p (getenv "NELISP_S615_BIN"))
                       (file-readable-p nelisp-eln-s615-test--eln)))
     (nelisp-eln-s615-test--run-tampered ,offset ,expected ,new ,reason)))

(nelisp-eln-s615-test--e2e
 nelisp-eln-s615-e2e-unauthenticated-call-target-rejected
 "The `Fsymbol_value' call's slot displacement: 1335 -> 1334."
 (+ nelisp-eln-s615-test--body-vaddr 34) #xb8 #xb0
 "leaf-instructions-not-admitted")

(nelisp-eln-s615-test--e2e
 nelisp-eln-s615-e2e-tampered-body-rejected
 "`test %rax,%rax' after the `Fsymbol_value' call -> `test %rax,%rcx'."
 (+ nelisp-eln-s615-test--body-vaddr 40) #xc0 #xc1
 "leaf-instructions-not-admitted")

(nelisp-eln-s615-test--e2e
 nelisp-eln-s615-e2e-wrong-type-slot-rejected
 "top_level_run's `mov 0x20(%rbp),%r8' -> `mov 0x8(%rbp),%r8' (slot 1, nil)."
 (+ nelisp-eln-s615-test--top-vaddr 50) #x20 #x08
 "metadata-outside-emitter-slice")

(nelisp-eln-s615-test--e2e
 nelisp-eln-s615-e2e-wrong-form-slot-rejected
 "top_level_run's Feval form load `mov 0x18(%rbp),%rdi' -> slot 1 (nil).
GNU itself evaluates nil harmlessly, so only admission can reject it."
 (+ nelisp-eln-s615-test--top-vaddr 31) #x18 #x08
 "eval-form-not-admitted")

(provide 'nelisp-eln-s615-constant-admission-test)

;;; nelisp-eln-s615-constant-admission-test.el ends here
