;;; nelisp-eln-s63-all-forms-admission-test.el --- S6.3 macroexp--all-forms admission -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Focused host tests for admitting the genuine GNU 31.1 (ba35c031)
;; artifact of vendor macroexp.el `macroexp--all-forms'
;; (tools/ai/eln-progress.org S6.3):
;;
;;   - its 784-byte body matches exactly the `accumulate-forms' template:
;;     eleven authenticated imports, the d_reloc constants nil,
;;     `macroexp--expand-all', t and `listp', the module `quitcounter' and
;;     the two `symbols_with_pos_enabled' reads (which must agree);
;;   - any single changed fixed byte, or a call through an unauthenticated
;;     freloc slot, is rejected;
;;   - the 93-byte `gnu-require-subr' registration code for an
;;     optional-argument function ((forms &optional skip): min 1, max 2,
;;     ephemeral vector [1 2 NAME C-NAME REST]) decodes arity 2/min 1, type
;;     slot 4, Feval lexenv slot 5 and form slot 3; its Feval form must be
;;     exactly `(require \\='macroexp)', anything else is refused.
;;
;; Tests needing the genuine artifact skip when it is absent.  The
;; end-to-end rejections run only when NELISP_S63_BIN names a standalone
;; binary.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-eln-registration)
(require 'nelisp-eln-emitter)

(defconst nelisp-eln-s63-test--eln
  (expand-file-name
   "~/.cache/tmp/s6-survey-lex/macroexp--all-forms/overlay/eln/31.1-ba35c031/gnu-macroexp--all-forms.eln")
  "The genuine artifact.")

(defconst nelisp-eln-s63-test--body-vaddr #x1100)
(defconst nelisp-eln-s63-test--top-vaddr #x1410)

(defun nelisp-eln-s63-test--file-bytes ()
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally nelisp-eln-s63-test--eln)
    (buffer-string)))

(defun nelisp-eln-s63-test--body ()
  (substring (nelisp-eln-s63-test--file-bytes)
             nelisp-eln-s63-test--body-vaddr
             (+ nelisp-eln-s63-test--body-vaddr 784)))

(defun nelisp-eln-s63-test--top ()
  (substring (nelisp-eln-s63-test--file-bytes)
             nelisp-eln-s63-test--top-vaddr
             (+ nelisp-eln-s63-test--top-vaddr 93)))

(defun nelisp-eln-s63-test--analyze (bytes)
  (nelisp-eln-tail-code-analyze-multi-import-call
   bytes nelisp-eln-s63-test--body-vaddr))

;;; Body template

(ert-deftest nelisp-eln-s63-genuine-body-matches ()
  (skip-unless (file-readable-p nelisp-eln-s63-test--eln))
  (let ((analysis (nelisp-eln-s63-test--analyze (nelisp-eln-s63-test--body))))
    (should (eq (plist-get analysis :shape) 'accumulate-forms))
    (should (equal (mapcar (lambda (i) (plist-get i :slot))
                           (plist-get analysis :imports))
                   '(1320 945 7 1354 1119 1209 1196 1300 0 13 14)))
    (should (equal (mapcar (lambda (d) (plist-get d :slot))
                           (plist-get analysis :data-relocations))
                   '(0 1 5 7)))
    (should (= (plist-get analysis :module-counter-vaddr) #x4264))
    (should (= (plist-get analysis :symbols-with-pos-got) #x3fa8))
    (should (= (nelisp-eln-native-subr-multi-arity analysis) 2))
    (should (= (nelisp-eln-native-subr-multi-min-arity analysis) 1))))

(ert-deftest nelisp-eln-s63-tampered-body-rejected ()
  "Every single changed fixed byte of the body matches no shape."
  (skip-unless (file-readable-p nelisp-eln-s63-test--eln))
  (let* ((bytes (nelisp-eln-s63-test--body))
         (template (plist-get (cdr (assq 'accumulate-forms
                                         nelisp-eln-tail-code--multi-import-shapes))
                              :template))
         (checked 0))
    (dotimes (i (length template))
      (when (aref template i)
        (let ((tampered (copy-sequence bytes)))
          (aset tampered i (logxor (aref tampered i) 1))
          (should-not (nelisp-eln-s63-test--analyze tampered))
          (setq checked (1+ checked)))))
    ;; 784 bytes minus 2 GOT loads, 2 swp loads and 6 counter accesses.
    (should (= checked (- 784 (* 10 4))))
    (should-not (nelisp-eln-s63-test--analyze (substring bytes 0 783)))))

(ert-deftest nelisp-eln-s63-displacement-tampering-rejected ()
  "The second `symbols_with_pos_enabled' read must reach the first's GOT
slot, and every counter access the same counter."
  (skip-unless (file-readable-p nelisp-eln-s63-test--eln))
  (dolist (offset '(282 335 586))
    (let ((tampered (copy-sequence (nelisp-eln-s63-test--body))))
      (aset tampered offset (logxor (aref tampered offset) 8))
      (should-not (nelisp-eln-s63-test--analyze tampered)))))

(ert-deftest nelisp-eln-s63-unauthenticated-call-target-rejected ()
  (skip-unless (file-readable-p nelisp-eln-s63-test--eln))
  (let ((tampered (copy-sequence (nelisp-eln-s63-test--body))))
    ;; `call *0x1d88(%rbp)' (945 `Ffuncall') -> `call *0x1d80(%rbp)' (944).
    (should (= (aref tampered #xbe) #x88))
    (aset tampered #xbe #x80)
    (should-not (nelisp-eln-s63-test--analyze tampered)))
  ;; The spec's fixed ports authenticate (the canonical-builtin MANY ports
  ;; need the standalone runtime; the e2e run covers them) ...
  (let ((spec (cdr (assq 'accumulate-forms
                         nelisp-eln-native-subr--multi-import-specs))))
    (dolist (port (plist-get spec :ports))
      (when (eq (nth 1 port) 'fixed)
        (should (plist-get (nelisp-eln-native-subr--multi-port-spec
                            "ba35c031" port)
                           :implementation))))
    ;; the nconc row is a runtime-services one, spread into arguments.
    (should (equal (funcall (plist-get
                             (nelisp-eln-native-subr--multi-port-spec
                              "ba35c031" '(1196 many 2 (lisp lisp) lisp))
                             :implementation)
                            (list 1 2) '(3))
                   '(1 2 3))))
  ;; ... an unimplemented slot, wrong arity or convention does not.
  (should (equal (cdr (should-error
                       (nelisp-eln-native-subr--multi-port-spec
                        "ba35c031" '(1334 fixed 1 (lisp) lisp))
                       :type 'nelisp-eln-native-subr-error))
                 '(multi-import-not-admitted unauthenticated-fixed-slot 1334)))
  (dolist (port '((1196 many 3 (lisp lisp lisp) lisp)
                  (1196 fixed 2 (lisp lisp) lisp)
                  (1354 fixed 2 (lisp lisp) lisp)))
    (should-error (nelisp-eln-native-subr--multi-port-spec "ba35c031" port)
                  :type 'nelisp-eln-native-subr-error)))

;;; Registration code

(defun nelisp-eln-s63-test--metadata (&optional min form)
  (list :abi-hash "ba35c031"
        :data-relocations
        (vector nil 'macroexp--expand-all 0 (or form '(require 'macroexp))
                '(function (t &optional t) t) t 'consp 'listp
                'symbol-with-pos-p)
        :ephemeral-data-relocations
        (vector (or min 1) 2 'macroexp--all-forms
                (nelisp-eln-emitter--symbol-name 'macroexp--all-forms)
                '(0 nil nil))
        :function-docs ["\n\n(fn FORMS &optional SKIP)"]
        :d-reloc-size 72 :d-reloc-eph-size 40))

(ert-deftest nelisp-eln-s63-registration-code-decodes ()
  (skip-unless (file-readable-p nelisp-eln-s63-test--eln))
  (let ((top (nelisp-eln-s63-test--top)))
    (should (nelisp-eln-registration--match-holed-template
             top nelisp-eln-registration--gnu-require-subr-opt-template
             nelisp-eln-registration--gnu-require-subr-holes))
    ;; It is not the fixed-layout thunk.
    (should-not (nelisp-eln-registration--match-holed-template
                 top nelisp-eln-registration--gnu-require-subr-template
                 nelisp-eln-registration--gnu-require-subr-holes))
    (should (= (nelisp-eln-registration--gnu-arity top 52) 2))
    (should (= (nelisp-eln-registration--gnu-arity top 61) 1))
    (should (= (nelisp-eln-registration--d-reloc-index top 27) 5))
    (should (= (nelisp-eln-registration--d-reloc-index top 31) 3))
    (should (= (nelisp-eln-registration--d-reloc-index top 50) 4))
    (dotimes (i (length top))
      (unless (nelisp-eln-registration--offset-holed-p
               i nelisp-eln-registration--gnu-require-subr-holes)
        (let ((tampered (copy-sequence top)))
          (aset tampered i (logxor (aref tampered i) 1))
          (should-not (nelisp-eln-registration--match-holed-template
                       tampered
                       nelisp-eln-registration--gnu-require-subr-opt-template
                       nelisp-eln-registration--gnu-require-subr-holes)))))))

(ert-deftest nelisp-eln-s63-optional-metadata-authenticated ()
  ;; S6.6's optional-arity layout: eph [MIN MAX NAME C-NAME REST].
  (let ((extra '(:type-index 4 :min-arity 1 :eph-offset 1))
        (metadata (nelisp-eln-s63-test--metadata)))
    (should (= (nelisp-eln-registration--metadata-data-count
                metadata 'gnu-require-subr 2 extra)
               9))
    ;; Wrong ephemeral vector shape, min or max.
    (dolist (bad (list (nelisp-eln-s63-test--metadata 2)
                       (plist-put (nelisp-eln-s63-test--metadata)
                                  :ephemeral-data-relocations
                                  (vector 1 3 'macroexp--all-forms
                                          (nelisp-eln-emitter--symbol-name
                                           'macroexp--all-forms)
                                          '(0 nil nil)))
                       (plist-put (nelisp-eln-s63-test--metadata)
                                  :ephemeral-data-relocations
                                  (vector 1 2 'a "b" '(0 nil nil) 0))))
      (should-error (nelisp-eln-registration--metadata-data-count
                     bad 'gnu-require-subr 2 extra)
                    :type 'nelisp-eln-registration-error))
    ;; A wrong type slot, or a minimum the type does not spell.
    (dolist (index '(0 1 3 5 9))
      (should-error (nelisp-eln-registration--metadata-data-count
                     metadata 'gnu-require-subr 2
                     (list :type-index index :min-arity 1 :eph-offset 1))
                    :type 'nelisp-eln-registration-error))
    (should-error (nelisp-eln-registration--metadata-data-count
                   metadata 'gnu-require-subr 2
                   '(:type-index 4 :min-arity 0 :eph-offset 1))
                  :type 'nelisp-eln-registration-error)
    (should-not (nelisp-eln-registration--verified-optional-subr-type-p
                 '(function (t t &optional) t) 1 2))
    (should (nelisp-eln-registration--verified-optional-subr-type-p
             '(function (t &optional t) t) 1 2))))

(ert-deftest nelisp-eln-s63-require-effect-authenticated ()
  (should (eq (nelisp-eln-registration--require-effect
               (nelisp-eln-s63-test--metadata) 3 5)
              'macroexp))
  (dolist (case (list
                 (list (nelisp-eln-s63-test--metadata) 3 1)
                 (list (nelisp-eln-s63-test--metadata) 6 5)
                 (list (nelisp-eln-s63-test--metadata nil '(require 'shell))
                       3 5)
                 (list (nelisp-eln-s63-test--metadata
                        nil '(require 'macroexp nil t))
                       3 5)
                 (list (nelisp-eln-s63-test--metadata nil '(load "macroexp"))
                       3 5)
                 (list (nelisp-eln-s63-test--metadata nil '(require macroexp))
                       3 5)))
    (should (eq (cadr (should-error
                       (apply #'nelisp-eln-registration--require-effect case)
                       :type 'nelisp-eln-registration-error))
                'eval-form-not-admitted))))

;;; End-to-end on a standalone binary (optional)

(defconst nelisp-eln-s63-test--root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name
                             default-directory))))
  "Repository root, captured while this file loads.")

(defun nelisp-eln-s63-test--run-tampered (offset expected new reason)
  "Run the S6 harness on a copy with file byte OFFSET (EXPECTED) set to
NEW and require a failure naming REASON."
  (let* ((dir (make-temp-file "s63-tamper" t))
         (eln (expand-file-name "gnu-macroexp--all-forms.eln" dir))
         (source (expand-file-name
                  "~/.cache/tmp/s6-survey-lex/macroexp--all-forms/macroexp--all-forms.el")))
    (unwind-protect
        (progn
          (copy-file nelisp-eln-s63-test--eln eln)
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
                    (cons (concat "NELISP_BIN=" (getenv "NELISP_S63_BIN"))
                          process-environment))
                   (default-directory nelisp-eln-s63-test--root)
                   (rc (call-process
                        "sh" nil t nil "test/nelisp-eln-s6-measure.sh"
                        "--eln" eln "--function" "macroexp--all-forms"
                        "--source" source
                        "--corpus" "test/fixtures/s6-corpus/macroexp--all-forms.el")))
              (should-not (eql rc 0))
              (should (string-match-p reason (buffer-string))))))
      (delete-directory dir t))))

(defmacro nelisp-eln-s63-test--e2e (name doc offset expected new reason)
  `(ert-deftest ,name ()
     ,doc
     (skip-unless (and (getenv "NELISP_S63_BIN")
                       (file-executable-p (getenv "NELISP_S63_BIN"))
                       (file-readable-p nelisp-eln-s63-test--eln)))
     (nelisp-eln-s63-test--run-tampered ,offset ,expected ,new ,reason)))

(nelisp-eln-s63-test--e2e
 nelisp-eln-s63-e2e-unauthenticated-call-target-rejected
 "The `Fsub1' call's slot displacement (a path this corpus never runs, so
the host phase still succeeds): 1300 -> 1299."
 (+ nelisp-eln-s63-test--body-vaddr #x2ea) #xa0 #x98
 "leaf-instructions-not-admitted")

(nelisp-eln-s63-test--e2e
 nelisp-eln-s63-e2e-tampered-body-rejected
 "A byte inside unexecuted alignment padding (a `nopw' displacement)."
 (+ nelisp-eln-s63-test--body-vaddr #x1bb) #x00 #x01
 "leaf-instructions-not-admitted")

(nelisp-eln-s63-test--e2e
 nelisp-eln-s63-e2e-non-allowlisted-feval-form-rejected
 "top_level_run's Feval form load `mov 0x18(%rbp),%rdi' -> d_reloc slot 2
\(the integer 0); GNU evaluates that harmlessly, so only admission can
reject it."
 (+ nelisp-eln-s63-test--top-vaddr 31) #x18 #x10
 "eval-form-not-admitted")

(nelisp-eln-s63-test--e2e
 nelisp-eln-s63-e2e-wrong-min-arity-rejected
 "register_subr's minimum-argument immediate 1 -> 2 (min == max)."
 (+ nelisp-eln-s63-test--top-vaddr 61) #x06 #x0a
 "top-level-instructions-not-admitted")

(provide 'nelisp-eln-s63-all-forms-admission-test)

;;; nelisp-eln-s63-all-forms-admission-test.el ends here
