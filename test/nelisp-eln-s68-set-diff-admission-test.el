;;; nelisp-eln-s68-set-diff-admission-test.el --- S6.8 cconv--set-diff admission -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Focused host tests for admitting the genuine GNU 31.1 (ba35c031)
;; artifact of vendor cconv.el `cconv--set-diff' (tools/ai/eln-progress.org
;; S6.8):
;;
;;   - its 308-byte body matches exactly the `set-difference' template:
;;     six authenticated imports (0 `wrong_type_argument', 13 `maybe_gc',
;;     14 `maybe_quit', 1217 `Fmemq', 1119 `Fcons', tail 1209
;;     `Fnreverse'), d_reloc[0] nil and d_reloc[4] `listp', and five
;;     direct accesses that all reach the module-local `quitcounter';
;;   - any single changed fixed byte, a call through an unauthenticated
;;     freloc slot, or a redirected counter access is rejected;
;;   - the counter address is admitted only as the artifact's own `.symtab'
;;     `quitcounter' object inside `.bss';
;;   - the 69-byte `gnu-verified-subr' registration code decodes arity 2
;;     and type slot 1, and a registration pointing its type load at any
;;     slot that does not hold a genuine fixed-arity-2 function type is
;;     refused.
;;
;; Tests needing the genuine artifact skip when it is absent.  The
;; end-to-end rejections run only when NELISP_S68_BIN names a standalone
;; binary.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-eln-registration)
(require 'nelisp-eln-emitter)

(defconst nelisp-eln-s68-test--eln
  (expand-file-name
   "~/.cache/tmp/s6-survey-lex/cconv--set-diff/overlay/eln/31.1-ba35c031/gnu-cconv--set-diff.eln")
  "The genuine artifact (sha256 7f4cedf5..fb1392).")

(defconst nelisp-eln-s68-test--body-vaddr #x1100)
(defconst nelisp-eln-s68-test--top-vaddr #x1240)

(defun nelisp-eln-s68-test--file-bytes ()
  "Return the genuine artifact's bytes as a unibyte string."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally nelisp-eln-s68-test--eln)
    (buffer-string)))

(defun nelisp-eln-s68-test--body ()
  "Return the genuine body (text is mapped at file offset = vaddr)."
  (substring (nelisp-eln-s68-test--file-bytes)
             nelisp-eln-s68-test--body-vaddr
             (+ nelisp-eln-s68-test--body-vaddr 308)))

(defun nelisp-eln-s68-test--top ()
  (substring (nelisp-eln-s68-test--file-bytes)
             nelisp-eln-s68-test--top-vaddr
             (+ nelisp-eln-s68-test--top-vaddr 69)))

(defun nelisp-eln-s68-test--analyze (bytes)
  (nelisp-eln-tail-code-analyze-multi-import-call
   bytes nelisp-eln-s68-test--body-vaddr))

;;; Body template

(ert-deftest nelisp-eln-s68-genuine-body-matches ()
  (skip-unless (file-readable-p nelisp-eln-s68-test--eln))
  (let ((analysis (nelisp-eln-s68-test--analyze (nelisp-eln-s68-test--body))))
    (should (eq (plist-get analysis :shape) 'set-difference))
    (should (eq (plist-get analysis :proof) :multi-import-call))
    (should (equal (mapcar (lambda (i) (plist-get i :slot))
                           (plist-get analysis :imports))
                   '(0 13 14 1217 1119 1209)))
    (should (= (plist-get (car (plist-get analysis :imports)) :got-vaddr)
               #x3fd8))
    (should (equal (mapcar (lambda (d) (list (plist-get d :slot)
                                             (plist-get d :got-vaddr)))
                           (plist-get analysis :data-relocations))
                   '((0 #x3fc8) (4 #x3fc8))))
    (should (= (plist-get analysis :module-counter-vaddr) #x41c4))
    (should-not (plist-get analysis :symbols-with-pos-got))))

(ert-deftest nelisp-eln-s68-tampered-body-rejected ()
  "Every single changed fixed byte of the body matches no shape."
  (skip-unless (file-readable-p nelisp-eln-s68-test--eln))
  (let* ((bytes (nelisp-eln-s68-test--body))
         (template (plist-get (cdr (assq 'set-difference
                                         nelisp-eln-tail-code--multi-import-shapes))
                              :template))
         (checked 0))
    (dotimes (i (length template))
      (when (aref template i)
        (let ((tampered (copy-sequence bytes)))
          (aset tampered i (logxor (aref tampered i) 1))
          (should-not (nelisp-eln-s68-test--analyze tampered))
          (setq checked (1+ checked)))))
    ;; 308 bytes minus two GOT and five counter displacements.
    (should (= checked (- 308 (* 7 4))))
    (should-not (nelisp-eln-s68-test--analyze (substring bytes 0 307)))))

(ert-deftest nelisp-eln-s68-unauthenticated-call-target-rejected ()
  "Calling a freloc slot other than the authenticated six is refused."
  (skip-unless (file-readable-p nelisp-eln-s68-test--eln))
  (let ((tampered (copy-sequence (nelisp-eln-s68-test--body))))
    ;; `call *0x2608(%rbp)' (1217 `Fmemq') -> `call *0x2600(%rbp)' (1216
    ;; `Fmemql', which has no NeLisp runtime-services implementation).
    (should (= (aref tampered #x52) #x08))
    (aset tampered #x52 #x00)
    (should-not (nelisp-eln-s68-test--analyze tampered)))
  ;; Every port of the admitted spec authenticates ...
  (let ((spec (cdr (assq 'set-difference
                         nelisp-eln-native-subr--multi-import-specs))))
    (should (= (nelisp-eln-native-subr-multi-arity
                (list :shape 'set-difference))
               2))
    (should (equal (plist-get spec :constants) '((0 . nil) (4 . listp))))
    (dolist (port (plist-get spec :ports))
      (should (plist-get (nelisp-eln-native-subr--multi-port-spec
                          "ba35c031" port)
                         :implementation))))
  ;; ... and an unimplemented slot does not.
  (should (equal (cdr (should-error
                        (nelisp-eln-native-subr--multi-port-spec
                         "ba35c031" '(1216 fixed 2 (lisp lisp) lisp))
                        :type 'nelisp-eln-native-subr-error))
                 '(multi-import-not-admitted unauthenticated-fixed-slot 1216))))

(ert-deftest nelisp-eln-s68-redirected-counter-rejected ()
  "Counter accesses must agree, and only on the real `quitcounter'."
  (skip-unless (file-readable-p nelisp-eln-s68-test--eln))
  (let ((body (nelisp-eln-s68-test--body)))
    ;; One access moved: the five no longer agree.
    (let ((tampered (copy-sequence body)))
      (aset tampered 97 (+ (aref tampered 97) 4))
      (should-not (nelisp-eln-s68-test--analyze tampered))))
  (let* ((bytes (nelisp-eln-s68-test--file-bytes))
         (symbols (plist-get (nelisp-eln-system-loader--file-symbols
                              nelisp-eln-s68-test--eln bytes)
                             :symbols)))
    (should (nelisp-eln-native-subr--module-counter-valid-p
             bytes #x41c4 "quitcounter" symbols))
    ;; `completed.0', the byte after the counter, `d_reloc' and a
    ;; misaligned address are all refused, as is another symbol name.
    (dolist (vaddr '(#x41c0 #x41c8 #x4200 #x41c5))
      (should-not (nelisp-eln-native-subr--module-counter-valid-p
                   bytes vaddr "quitcounter" symbols)))
    (should-not (nelisp-eln-native-subr--module-counter-valid-p
                 bytes #x41c0 "completed.0" symbols))))

;;; Registration code

(defun nelisp-eln-s68-test--metadata (&optional eph0)
  (list :abi-hash "ba35c031"
        :data-relocations
        (vector nil '(function (t t) t) t 'consp 'listp 'symbol-with-pos-p)
        :ephemeral-data-relocations
        (vector (or eph0 2) 'cconv--set-diff
                (nelisp-eln-emitter--symbol-name 'cconv--set-diff)
                '(0 nil nil))
        :function-docs
        ["Return elements of set S1 that are not in set S2.\n\n(fn S1 S2)"]
        :d-reloc-size 48 :d-reloc-eph-size 32))

(ert-deftest nelisp-eln-s68-registration-code-decodes ()
  (skip-unless (file-readable-p nelisp-eln-s68-test--eln))
  (let ((top (nelisp-eln-s68-test--top)))
    (should (nelisp-eln-registration--match-holed-template
             top nelisp-eln-registration--gnu-verified-subr-template
             nelisp-eln-registration--gnu-verified-subr-holes))
    (should (= (nelisp-eln-registration--gnu-arity top 45) 2))
    (should (= (nelisp-eln-registration--gnu-arity top 54) 2))
    (should (= (nelisp-eln-registration--d-reloc-index top 43) 1))
    ;; Not the 68-byte three-byte-type-load thunk.
    (should-not (= (length top) 68))
    ;; Any changed fixed byte is no shape.
    (dotimes (i (length top))
      (unless (nelisp-eln-registration--offset-holed-p
               i nelisp-eln-registration--gnu-verified-subr-holes)
        (let ((tampered (copy-sequence top)))
          (aset tampered i (logxor (aref tampered i) 1))
          (should-not (nelisp-eln-registration--match-holed-template
                       tampered nelisp-eln-registration--gnu-verified-subr-template
                       nelisp-eln-registration--gnu-verified-subr-holes)))))))

(ert-deftest nelisp-eln-s68-type-slot-authenticated ()
  (let ((metadata (nelisp-eln-s68-test--metadata)))
    (should (= (nelisp-eln-registration--metadata-data-count
                metadata 'gnu-verified-subr 2 '(:type-index 1))
               6))
    ;; Wrong type slot: nil, t, a predicate symbol, out of range.
    (dolist (index '(0 2 4 6))
      (should (eq (cadr (should-error
                         (nelisp-eln-registration--metadata-data-count
                          metadata 'gnu-verified-subr 2
                          (list :type-index index))
                         :type 'nelisp-eln-registration-error))
                  'metadata-outside-emitter-slice)))
    ;; A type whose argument count disagrees with the registered arity.
    (should-error (nelisp-eln-registration--metadata-data-count
                   metadata 'gnu-verified-subr 1 '(:type-index 1))
                  :type 'nelisp-eln-registration-error)
    ;; The top-level-unused eph word must be the genuine arity-2 value.
    (should-error (nelisp-eln-registration--metadata-data-count
                   (nelisp-eln-s68-test--metadata 1)
                   'gnu-verified-subr 2 '(:type-index 1))
                  :type 'nelisp-eln-registration-error))
  (should (nelisp-eln-registration--verified-subr-type-p
           '(function (t t) t) 2))
  (should-not (nelisp-eln-registration--verified-subr-type-p
               '(function (t &optional t) t) 2))
  (should-not (nelisp-eln-registration--verified-subr-type-p
               '(function (t &rest t) t) 2)))

;;; End-to-end on a standalone binary (optional)

(defconst nelisp-eln-s68-test--root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name
                             default-directory))))
  "Repository root, captured while this file loads.")

(defun nelisp-eln-s68-test--run-tampered (offset expected new reason)
  "Run the S6 harness on a copy with file byte OFFSET (EXPECTED) set to
NEW and require a failure naming REASON."
  (let* ((dir (make-temp-file "s68-tamper" t))
         (eln (expand-file-name "gnu-cconv--set-diff.eln" dir))
         (source (expand-file-name
                  "~/.cache/tmp/s6-survey-lex/cconv--set-diff/cconv--set-diff.el")))
    (unwind-protect
        (progn
          (copy-file nelisp-eln-s68-test--eln eln)
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
                    (cons (concat "NELISP_BIN=" (getenv "NELISP_S68_BIN"))
                          process-environment))
                   (default-directory nelisp-eln-s68-test--root)
                   (rc (call-process
                        "sh" nil t nil "test/nelisp-eln-s6-measure.sh"
                        "--eln" eln "--function" "cconv--set-diff"
                        "--source" source
                        "--corpus" "test/fixtures/s6-corpus/cconv--set-diff.el")))
              (should-not (eql rc 0))
              (should (string-match-p reason (buffer-string))))))
      (delete-directory dir t))))

(ert-deftest nelisp-eln-s68-e2e-tampered-body-rejected ()
  (skip-unless (and (getenv "NELISP_S68_BIN")
                    (file-executable-p (getenv "NELISP_S68_BIN"))
                    (file-readable-p nelisp-eln-s68-test--eln)))
  ;; The `Fmemq' call's slot displacement: 1217 -> 1216 (`Fmemql').
  (nelisp-eln-s68-test--run-tampered
   (+ nelisp-eln-s68-test--body-vaddr #x52) #x08 #x00
   "leaf-instructions-not-admitted"))

(ert-deftest nelisp-eln-s68-e2e-wrong-type-slot-rejected ()
  (skip-unless (and (getenv "NELISP_S68_BIN")
                    (file-executable-p (getenv "NELISP_S68_BIN"))
                    (file-readable-p nelisp-eln-s68-test--eln)))
  ;; top_level_run's `mov 0x8(%rcx),%r8' -> `mov 0x10(%rcx),%r8' (slot 2, `t').
  (nelisp-eln-s68-test--run-tampered
   (+ nelisp-eln-s68-test--top-vaddr 43) #x08 #x10
   "metadata-outside-emitter-slice"))

(provide 'nelisp-eln-s68-set-diff-admission-test)

;;; nelisp-eln-s68-set-diff-admission-test.el ends here
