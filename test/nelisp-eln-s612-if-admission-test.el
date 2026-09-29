;;; nelisp-eln-s612-if-admission-test.el --- S6.12 byte-compile-if admission -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Focused host tests for admitting the genuine GNU 31.1 (ba35c031)
;; artifact of vendor bytecomp.el `byte-compile-if'
;; (tools/ai/eln-progress.org S6.12):
;;
;;   - its top_level_run registers TWO native anonymous lambdas through
;;     `Fcomp__register_lambda' (slot 1031), then evaluates
;;     `(require \\='bytecomp)' (slot 947) and registers the subr (slot
;;     1030): the `gnu-lambda-require-subr' profile, whose role plan is
;;     (lambda lambda require register);
;;   - the three bodies (102-byte `lambda-nth-form', 90-byte
;;     `lambda-cdr-form', 1159-byte `if-form') match exact templates with
;;     every import slot and d_reloc constant authenticated, and the
;;     first lambda is a `jmp' thunk to the exported lambda 0;
;;   - any single changed fixed byte, a lambda registered through a
;;     different slot or d_reloc index, a call past the admitted role
;;     sequence, a MANY port that is not the authenticated `Fmake_closure'
;;     row, and a byte-code constant that differs are all rejected.
;;
;; Tests needing the genuine artifact skip when it is absent.  The
;; end-to-end rejections run only when NELISP_S612_BIN names a standalone
;; binary.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-eln-registration)
(require 'nelisp-eln-emitter)

(defconst nelisp-eln-s612-test--eln
  (expand-file-name
   "~/.cache/tmp/s6-survey-lex/byte-compile-if/overlay/eln/31.1-ba35c031/gnu-byte-compile-if.eln")
  "The genuine artifact (sha256 b82d7066..bb5043).")

(defconst nelisp-eln-s612-test--bodies
  '((lambda-nth-form #x1100 102 (1220 1335 945) (0 3))
    (lambda-cdr-form #x1180 90 (0 1335 945) (3 4 24))
    (if-form #x11e0 1159 (945 1113 1119 10 1335 0)
             (0 3 5 7 8 9 11 12 13 14 15 16 24)))
  "(SHAPE VADDR SIZE IMPORT-SLOTS DATA-SLOTS) of the three genuine bodies.")

(defconst nelisp-eln-s612-test--top-vaddr #x1670)

(defun nelisp-eln-s612-test--file-bytes ()
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally nelisp-eln-s612-test--eln)
    (buffer-string)))

(defun nelisp-eln-s612-test--slice (vaddr size)
  (substring (nelisp-eln-s612-test--file-bytes) vaddr (+ vaddr size)))

(defun nelisp-eln-s612-test--analyze (bytes vaddr)
  (nelisp-eln-tail-code-analyze-multi-import-call bytes vaddr))

;;; Body templates

(ert-deftest nelisp-eln-s612-genuine-bodies-match ()
  (skip-unless (file-readable-p nelisp-eln-s612-test--eln))
  (dolist (body nelisp-eln-s612-test--bodies)
    (pcase-let ((`(,shape ,vaddr ,size ,imports ,data) body))
      (let ((analysis (nelisp-eln-s612-test--analyze
                       (nelisp-eln-s612-test--slice vaddr size) vaddr)))
        (should (eq (plist-get analysis :shape) shape))
        (should (eq (plist-get analysis :proof) :multi-import-call))
        (should (equal (mapcar (lambda (i) (plist-get i :slot))
                               (plist-get analysis :imports))
                       imports))
        (should (cl-every (lambda (i) (= (plist-get i :got-vaddr) #x3fd8))
                          (plist-get analysis :imports)))
        (should (equal (mapcar (lambda (d) (plist-get d :slot))
                               (plist-get analysis :data-relocations))
                       data))
        (should (cl-every (lambda (d) (= (plist-get d :got-vaddr) #x3fc8))
                          (plist-get analysis :data-relocations)))
        (should-not (plist-get analysis :module-counter-vaddr))
        (should-not (plist-get analysis :symbols-with-pos-got))))))

(ert-deftest nelisp-eln-s612-tampered-body-byte-rejected ()
  "Every fixed byte of every body, flipped one at a time, is rejected."
  (skip-unless (file-readable-p nelisp-eln-s612-test--eln))
  (dolist (body nelisp-eln-s612-test--bodies)
    (pcase-let ((`(,shape ,vaddr ,size ,_ ,_) body))
      (let* ((bytes (nelisp-eln-s612-test--slice vaddr size))
             (template (plist-get
                        (cdr (assq shape
                                   nelisp-eln-tail-code--multi-import-shapes))
                        :template))
             (holes (* 4 (length (plist-get
                                  (cdr (assq shape
                                             nelisp-eln-tail-code--multi-import-shapes))
                                  :gots))))
             (checked 0))
        (should (= (length template) size))
        (dotimes (i size)
          (when (aref template i)
            (let ((tampered (copy-sequence bytes)))
              (aset tampered i (logxor (aref tampered i) 1))
              (should-not (nelisp-eln-s612-test--analyze tampered vaddr))
              (setq checked (1+ checked)))))
        (should (= checked (- size holes)))))))

(ert-deftest nelisp-eln-s612-got-loads-must-agree ()
  "A second d_reloc GOT load reaching another slot is rejected."
  (skip-unless (file-readable-p nelisp-eln-s612-test--eln))
  (let ((tampered (copy-sequence
                   (nelisp-eln-s612-test--slice #x11e0 1159))))
    ;; The third d_reloc load's displacement is at body offset 875.
    (aset tampered 875 (logxor (aref tampered 875) 8))
    (should-not (nelisp-eln-s612-test--analyze tampered #x11e0))))

(ert-deftest nelisp-eln-s612-port-specs-authenticate ()
  (dolist (shape '(lambda-nth-form lambda-cdr-form if-form))
    (let ((spec (cdr (assq shape nelisp-eln-native-subr--multi-import-specs))))
      ;; The host Emacs has no `(builtin funcall)' cell, so only the fixed
      ;; runtime-service ports and the `Fmake_closure' service row are
      ;; resolved here (the canonical-builtin MANY rows run on NeLisp).
      (dolist (port (plist-get spec :ports))
        (when (or (eq (nth 1 port) 'fixed) (eql (nth 0 port) 1113))
          (should (plist-get (nelisp-eln-native-subr--multi-port-spec
                              "ba35c031" port)
                             :implementation)))))
    (should (= (nelisp-eln-native-subr-multi-arity (list :shape shape))
               (if (eq shape 'if-form) 1 0))))
  ;; `Fmake_closure' (slot 1113) is one authenticated MANY service row:
  ;; exactly two arguments, and only as a MANY convention.
  (dolist (port '((1113 many 3 (lisp lisp lisp) handle)
                  (1113 many (2 3) (lisp lisp lisp) handle)
                  (1113 fixed 2 (lisp lisp) lisp)
                  (1114 many 2 (lisp lisp) lisp)))
    (should-error (nelisp-eln-native-subr--multi-port-spec "ba35c031" port)
                  :type 'nelisp-eln-native-subr-error))
  (should (equal (nelisp-eln-runtime-services-fmake-closure
                  (make-byte-code 0 "" [nil] 1) 42)
                 (make-byte-code 0 "" [42] 1)))
  (should-error (nelisp-eln-runtime-services-fmake-closure 'not-bytecode)
                :type 'wrong-type-argument))

(ert-deftest nelisp-eln-s612-bytecode-constant-identity ()
  (let ((spec '(:bytecode 0 (194 195 192 56 9 34 135)
                          [V0 byte-compile--for-effect byte-compile-form 2]
                          3)))
    (should (nelisp-eln-native-subr--multi-constant-matches-p
             (make-byte-code 0 (unibyte-string 194 195 192 56 9 34 135)
                             [V0 byte-compile--for-effect byte-compile-form 2]
                             3)
             spec))
    ;; A changed byte, constant, depth or arglist, or a non-function.
    (dolist (actual (list (make-byte-code 0 (unibyte-string 194 195 192 57 9 34 135)
                                          [V0 byte-compile--for-effect
                                              byte-compile-form 2] 3)
                          (make-byte-code 0 (unibyte-string 194 195 192 56 9 34 135)
                                          [V0 byte-compile--for-effect
                                              byte-compile-body 2] 3)
                          (make-byte-code 0 (unibyte-string 194 195 192 56 9 34 135)
                                          [V0 byte-compile--for-effect
                                              byte-compile-form 2] 4)
                          (make-byte-code 257 (unibyte-string 194 195 192 56 9 34 135)
                                          [V0 byte-compile--for-effect
                                              byte-compile-form 2] 3)
                          nil 'byte-compile-form))
      (should-not (nelisp-eln-native-subr--multi-constant-matches-p
                   actual spec)))))

;;; Registration code (`gnu-lambda-require-subr')

(defun nelisp-eln-s612-test--top ()
  (nelisp-eln-s612-test--slice nelisp-eln-s612-test--top-vaddr 192))

(ert-deftest nelisp-eln-s612-registration-code-decodes ()
  (skip-unless (file-readable-p nelisp-eln-s612-test--eln))
  (let ((top (nelisp-eln-s612-test--top)))
    (should (nelisp-eln-registration--match-holed-template
             top nelisp-eln-registration--gnu-lambda-require-subr-template
             nelisp-eln-registration--gnu-lambda-require-subr-holes))
    ;; Lambda arities 0, the registered subr's arity 1.
    (should (equal (mapcar (lambda (o) (nelisp-eln-registration--gnu-arity top o))
                           '(3 8 84 97 149 166))
                   '(0 0 0 0 1 1)))
    ;; Lambda d_reloc indices 17 and 18.
    (should (= (nelisp-eln-registration--gnu-fixnum-immediate top 64) 17))
    (should (= (nelisp-eln-registration--gnu-fixnum-immediate top 106) 18))
    ;; Lambda type slot 20, subr type slot 21, Feval lexenv 22, form 19.
    (should (equal (mapcar (lambda (o)
                             (nelisp-eln-registration--d-reloc-index32 top o))
                           '(52 92 157 121 129))
                   '(20 20 21 22 19)))
    ;; No other (earlier) profile's template admits it.
    (dolist (template (list nelisp-eln-registration--gnu-require-subr-template
                            nelisp-eln-registration--gnu-eval-subr-pair-template
                            nelisp-eln-registration--gnu-eval-subr-template))
      (should-not (= (length template) (length top))))))

(ert-deftest nelisp-eln-s612-tampered-registration-code-rejected ()
  "Every fixed byte of top_level_run, including each import slot
displacement, flipped one at a time, leaves the template unmatched."
  (skip-unless (file-readable-p nelisp-eln-s612-test--eln))
  (let* ((top (nelisp-eln-s612-test--top))
         (holes nelisp-eln-registration--gnu-lambda-require-subr-holes)
         (checked 0))
    (dotimes (i (length top))
      (unless (nelisp-eln-registration--offset-holed-p i holes)
        (let ((tampered (copy-sequence top)))
          (aset tampered i (logxor (aref tampered i) 1))
          (should-not (nelisp-eln-registration--match-holed-template
                       tampered
                       nelisp-eln-registration--gnu-lambda-require-subr-template
                       holes))
          (setq checked (1+ checked)))))
    (should (= checked (- 192 (* 4 (length holes)))))
    ;; In particular the three call slots: 1031 twice, 947, 1030.
    (should (equal (mapcar (lambda (o) (+ (aref top o) (* 256 (aref top (1+ o)))))
                           '(71 113 136 177))
                   '(#x2038 #x2038 #x1d98 #x2030)))))

(defun nelisp-eln-s612-test--metadata ()
  (list :abi-hash "ba35c031"
        :data-relocations
        (vector 'byte-compile-form 2 0 'byte-compile--for-effect
                'byte-compile-body 'byte-compile-make-tag nil
                'byte-compile-goto 'byte-goto-if-nil
                'byte-compile--maybe-guarded 'make-closure nil
                'byte-goto 'byte-compile-out-tag 'not nil
                'byte-goto-if-nil-else-pop "#$" "#$"
                '(require 'bytecomp) '(function nil t)
                '(function (t) null) t 'consp 'listp 'symbol-with-pos-p)
        :ephemeral-data-relocations
        (vector (concat nelisp-eln-registration--lambda-c-name-prefix "1")
                '(0 nil nil)
                (concat nelisp-eln-registration--lambda-c-name-prefix "2")
                '(1 nil nil) 1 'byte-compile-if
                (nelisp-eln-emitter--symbol-name 'byte-compile-if)
                '(2 nil nil))
        :function-docs [nil nil "\n\n(fn FORM)"]
        :d-reloc-size 208 :d-reloc-eph-size 64))

(defconst nelisp-eln-s612-test--extra
  '(:type-index 21 :lambda-type-index 20 :lexenv-index 22 :form-index 19
    :lambda-idx1 17 :lambda-idx2 18 :lambda-arity 0))

(ert-deftest nelisp-eln-s612-metadata-envelope ()
  (let ((metadata (nelisp-eln-s612-test--metadata)))
    (should (= (nelisp-eln-registration--metadata-data-count
                metadata 'gnu-lambda-require-subr 1
                nelisp-eln-s612-test--extra)
               26))
    (should (eq (nelisp-eln-registration--require-effect metadata 19 22)
                'bytecomp))
    ;; A lambda registered into a slot that is not a "#$" placeholder, into
    ;; the same slot twice, or over the Feval / type slots is refused.
    (dolist (extra (list (plist-put (copy-sequence nelisp-eln-s612-test--extra)
                                    :lambda-idx1 19)
                         (plist-put (copy-sequence nelisp-eln-s612-test--extra)
                                    :lambda-idx2 21)
                         (plist-put (copy-sequence nelisp-eln-s612-test--extra)
                                    :lambda-idx2 0)
                         (plist-put (copy-sequence nelisp-eln-s612-test--extra)
                                    :lambda-idx1 99)
                         (plist-put (copy-sequence nelisp-eln-s612-test--extra)
                                    :lambda-type-index 21)
                         (plist-put (copy-sequence nelisp-eln-s612-test--extra)
                                    :lambda-type-index 22)
                         (plist-put (copy-sequence nelisp-eln-s612-test--extra)
                                    :type-index 20)
                         (plist-put (copy-sequence nelisp-eln-s612-test--extra)
                                    :lambda-arity 1)))
      (should (eq (cadr (should-error
                         (nelisp-eln-registration--metadata-data-count
                          metadata 'gnu-lambda-require-subr 1 extra)
                         :type 'nelisp-eln-registration-error))
                  'metadata-outside-emitter-slice)))
    ;; Wrong lambda C names or descriptors are refused.
    (dolist (edit '((0 . "F616e6f6e796d6f75732d6c616d626461_anonymous_lambda_x")
                    (0 . "some_other_function")
                    (1 . (0 t nil))
                    (3 . (2 nil nil))
                    (4 . 2)
                    (7 . (0 nil nil))))
      (let ((bad (copy-sequence (nelisp-eln-s612-test--metadata))))
        (let ((eph (copy-sequence (plist-get bad :ephemeral-data-relocations))))
          (aset eph (car edit) (cdr edit))
          (setq bad (plist-put bad :ephemeral-data-relocations eph)))
        (should (eq (cadr (should-error
                           (nelisp-eln-registration--metadata-data-count
                            bad 'gnu-lambda-require-subr 1
                            nelisp-eln-s612-test--extra)
                           :type 'nelisp-eln-registration-error))
                    'metadata-outside-emitter-slice))))))

(ert-deftest nelisp-eln-s612-lambda-c-names ()
  (let ((p nelisp-eln-registration--lambda-c-name-prefix))
    (should (nelisp-eln-registration--lambda-c-name-p (concat p "2") "2"))
    (should (nelisp-eln-registration--lambda-c-name-p (concat p "12")))
    (should-not (nelisp-eln-registration--lambda-c-name-p (concat p "2") "1"))
    (should-not (nelisp-eln-registration--lambda-c-name-p p))
    (should-not (nelisp-eln-registration--lambda-c-name-p (concat p "2x")))
    (should-not (nelisp-eln-registration--lambda-c-name-p "zerop"))
    (should-not (nelisp-eln-registration--lambda-c-name-p nil))))

;;; Role sequence: nothing beyond the admitted calls

(defun nelisp-eln-s612-test--owner ()
  (let ((owner (make-vector nelisp-eln-registration--owner-size nil)))
    (aset owner 18 (list :role-sequence '(lambda lambda require register)
                         :lambdas (list (list :idx 17 :c-name "a" :arity 0)
                                        (list :idx 18 :c-name "b" :arity 0))))
    owner))

(ert-deftest nelisp-eln-s612-role-sequence ()
  (let ((owner (nelisp-eln-s612-test--owner)))
    (let ((roles nil))
      (dolist (n '(1 2 3 4 5 6))
        (let ((nelisp-eln-registration--call-index n))
          (push (nelisp-eln-registration--call-role owner) roles)))
      (should (equal (nreverse roles)
                     '(lambda lambda require register nil nil))))
    (let ((ordinals nil))
      (dolist (n '(1 2))
        (let ((nelisp-eln-registration--call-index n))
          (push (nelisp-eln-registration--lambda-ordinal owner) ordinals)))
      (should (equal (nreverse ordinals) '(1 2))))
    (let ((nelisp-eln-registration--call-index 4))
      (should (= (nelisp-eln-registration--register-ordinal owner) 1)))))

(ert-deftest nelisp-eln-s612-extra-registration-rejected ()
  "A call past the fourth admitted one (an extra registration) is refused."
  (let* ((owner (nelisp-eln-s612-test--owner))
         (nelisp-eln-registration--owners (list owner))
         (nelisp-eln-registration--active-owner owner)
         (nelisp-eln-registration--call-index 4))
    (should (eq (car (should-error (nelisp-eln-registration--callback 0)
                                   :type 'nelisp-eln-registration-error))
                'nelisp-eln-registration-error))
    (should (eq (cadr nelisp-eln-registration--last-callback-error)
                'callback-sequence-not-admitted))))

(ert-deftest nelisp-eln-s612-unadmitted-lambda-callback-rejected ()
  "A `register_lambda' callback with no owner, a third ordinal, or an
unexpected owner state is refused before anything is built."
  (let ((nelisp-eln-registration--active-owner nil))
    (cl-letf (((symbol-function 'nelisp-eln-abi-read-word)
               (lambda (_address _offset) 0)))
      (should (eq (cadr (should-error
                         (nelisp-eln-registration--register-lambda-callback
                          #x10000 1)
                         :type 'nelisp-eln-registration-error))
                  'callback-arguments-not-admitted))))
  (let* ((owner (nelisp-eln-s612-test--owner))
         (nelisp-eln-registration--active-owner owner))
    (cl-letf (((symbol-function 'nelisp-eln-abi-read-word)
               (lambda (_address _offset) 0)))
      (dolist (ordinal '(0 3 4))
        (should (eq (cadr (should-error
                           (nelisp-eln-registration--register-lambda-callback
                            #x10000 ordinal)
                           :type 'nelisp-eln-registration-error))
                    'callback-arguments-not-admitted))))))

;;; End-to-end on a standalone binary (optional)

(defconst nelisp-eln-s612-test--root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name
                             default-directory))))
  "Repository root, captured while this file loads.")

(defun nelisp-eln-s612-test--run-tampered (offset expected new reason)
  "Run the S6 harness on a copy with file byte OFFSET (EXPECTED) set to
NEW and require a failure naming REASON."
  (let* ((dir (make-temp-file "s612-tamper" t))
         (eln (expand-file-name "gnu-byte-compile-if.eln" dir))
         (source (expand-file-name
                  "~/.cache/tmp/s6-survey-lex/byte-compile-if/byte-compile-if.el")))
    (unwind-protect
        (progn
          (copy-file nelisp-eln-s612-test--eln eln)
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
                    (cons (concat "NELISP_BIN=" (getenv "NELISP_S612_BIN"))
                          process-environment))
                   (default-directory nelisp-eln-s612-test--root)
                   (rc (call-process
                        "sh" nil t nil "test/nelisp-eln-s6-measure.sh"
                        "--eln" eln "--function" "byte-compile-if"
                        "--source" source
                        "--corpus" "test/fixtures/s6-corpus/byte-compile-if.el"
                        "--wrapper"
                        "test/fixtures/s6-corpus/byte-compile-if.wrapper.el"
                        "--allow-stderr")))
              (should-not (eql rc 0))
              (should (string-match-p reason (buffer-string))))))
      (delete-directory dir t))))

(defmacro nelisp-eln-s612-test--e2e (name doc offset expected new reason)
  `(ert-deftest ,name ()
     ,doc
     (skip-unless (and (getenv "NELISP_S612_BIN")
                       (file-executable-p (getenv "NELISP_S612_BIN"))
                       (file-readable-p nelisp-eln-s612-test--eln)))
     (nelisp-eln-s612-test--run-tampered ,offset ,expected ,new ,reason)))

(nelisp-eln-s612-test--e2e
 nelisp-eln-s612-e2e-tampered-lambda-2-body-rejected
 "The second lambda's `mov $2,%esi' immediate."
 #x1182 #x02 #x06 "leaf-instructions-not-admitted")

(nelisp-eln-s612-test--e2e
 nelisp-eln-s612-e2e-tampered-lambda-0-body-rejected
 "The first lambda's (jmp target) body `mov $2,%esi' immediate."
 #x1103 #x02 #x06 "leaf-instructions-not-admitted")

(nelisp-eln-s612-test--e2e
 nelisp-eln-s612-e2e-tampered-lambda-thunk-rejected
 "The first registered lambda's `jmp rel8' opcode."
 #x1170 #xeb #xe9 "lambda-thunk-not-admitted")

(nelisp-eln-s612-test--e2e
 nelisp-eln-s612-e2e-tampered-main-body-rejected
 "The subr body's first `push %r15' -> `push %r14'."
 #x11e1 #x57 #x56 "leaf-instructions-not-admitted")

;; (Redirecting the first call to slot 1030 crashes the host Emacs phase of
;; the harness, which really loads the tampered artifact; the fixed-byte
;; unit test above covers every slot displacement.)
(nelisp-eln-s612-test--e2e
 nelisp-eln-s612-e2e-lambda-via-unauthenticated-slot-rejected
 "The second `register_lambda' call goes through slot 1032, not 1031."
 #x16e1 #x38 #x40 "top-level-instructions-not-admitted")

(nelisp-eln-s612-test--e2e
 nelisp-eln-s612-e2e-lambdas-into-one-slot-rejected
 "Both lambdas registered into d_reloc[17]."
 #x16da #x4a #x46 "top-level-instructions-not-admitted")

(nelisp-eln-s612-test--e2e
 nelisp-eln-s612-e2e-lambda-into-form-slot-rejected
 "The second lambda registered over d_reloc[19] (the require form)."
 #x16da #x4a #x4e "metadata-outside-emitter-slice")

(provide 'nelisp-eln-s612-if-admission-test)

;;; nelisp-eln-s612-if-admission-test.el ends here
