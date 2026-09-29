;;; nelisp-eln-s69-lambda-admission-test.el --- S6.9 byte-compile-lambda admission -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Focused tests for the admission steps of the genuine GNU 31.1
;; (ba35c031) artifact of vendor bytecomp.el `byte-compile-lambda'
;; (tools/ai/eln-progress.org S6.9; sha256 a4bf0b86..a19358):
;;
;;   - its `.text' crosses a 4 KiB boundary, so `.got.plt' sits one page
;;     later than in the smaller artifacts and every GOT-relative
;;     displacement of the fixed `.init'/`.plt'/`.plt.got'/CRT-stub bytes
;;     is one page larger: the pre-dlopen validator authenticates them
;;     against the file's own `.got.plt' page shift, still byte-exactly;
;;   - its 102-byte top_level_run is the `require'+`register_subr'
;;     skeleton with four-byte d_reloc displacements and `&optional'
;;     arity (1 . 2);
;;   - the 3909-byte body matches one exact template, and the local
;;     80-byte `maybe_gc_quit' helper before it another;
;;   - any single changed fixed byte, an unauthenticated call slot or a
;;     wrong type slot is rejected.
;;
;; The genuine artifact tests skip when it is absent.  The end-to-end
;; rejections run only when NELISP_S69_BIN names a standalone binary.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-eln-registration)

(defconst nelisp-eln-s69-test--eln
  (expand-file-name
   "~/.cache/tmp/s6-survey-lex/byte-compile-lambda/overlay/eln/31.1-ba35c031/gnu-byte-compile-lambda.eln")
  "The genuine artifact.")

(defconst nelisp-eln-s69-test--body-vaddr #x1150)
(defconst nelisp-eln-s69-test--body-size 3909)
(defconst nelisp-eln-s69-test--top-vaddr #x20a0)

(defun nelisp-eln-s69-test--file-bytes ()
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally nelisp-eln-s69-test--eln)
    (buffer-string)))

(defun nelisp-eln-s69-test--slice (vaddr size)
  (substring (nelisp-eln-s69-test--file-bytes) vaddr (+ vaddr size)))

(defun nelisp-eln-s69-test--shape ()
  (cdr (assq 'lambda-form nelisp-eln-tail-code--multi-import-shapes)))

;;; Pre-dlopen executable regions (GOT page shift)

(ert-deftest nelisp-eln-s69-preopen-validator-admits-genuine-artifact ()
  (skip-unless (file-readable-p nelisp-eln-s69-test--eln))
  (let ((bytes (nelisp-eln-s69-test--file-bytes)))
    (should (= (nelisp-eln-registration--layout-shift-in-bytes bytes) #x1000))
    ;; Returns nil and signals `preopen-executable-region-not-admitted' on
    ;; failure.
    (nelisp-eln-registration--validate-preopen bytes)))

(ert-deftest nelisp-eln-s69-preopen-validator-rejects-tampered-got-fields ()
  "Each shifted GOT displacement field, and each fixed byte around it, is
still byte-exact: flipping one bit of any .init/.plt/.plt.got/CRT-stub
byte is refused."
  (skip-unless (file-readable-p nelisp-eln-s69-test--eln))
  (let ((bytes (nelisp-eln-s69-test--file-bytes)) (checked 0))
    (dolist (region '((#x1000 . 23) (#x1020 . 16) (#x1030 . 8)))
      (dotimes (i (cdr region))
        (let ((tampered (copy-sequence bytes)))
          (aset tampered (+ (car region) i)
                (logxor (aref tampered (+ (car region) i)) 1))
          (should (eq (cadr (should-error
                             (nelisp-eln-registration--validate-preopen tampered)
                             :type 'nelisp-eln-registration-error))
                      'preopen-executable-region-not-admitted))
          (setq checked (1+ checked)))))
    ;; The four shifted CRT-stub fields.
    (dolist (field nelisp-eln-registration--crt-stub-got-fields)
      (let ((tampered (copy-sequence bytes)))
        (aset tampered (+ #x1040 field 1)
              (logxor (aref tampered (+ #x1040 field 1)) #x10))
        (should-error (nelisp-eln-registration--validate-preopen tampered)
                      :type 'nelisp-eln-registration-error)
        (setq checked (1+ checked))))
    (should (> checked 40))))

(ert-deftest nelisp-eln-s69-layout-shift-is-authenticated ()
  "The shift must be a non-negative whole number of pages; templates for
shift 0 still admit the small-artifact bytes and shifted ones do not."
  (let ((t0 (nelisp-eln-registration--fixed-templates 0))
        (t1 (nelisp-eln-registration--fixed-templates #x1000)))
    (should (equal (nth 0 t0) nelisp-eln-registration--init-template))
    (should-not (equal (nth 0 t1) (nth 0 t0)))
    (should (= (aref (nth 0 t1) 8) (+ 16 (aref (nth 0 t0) 8))))
    ;; The plt/plt.got/crt templates move only at their listed fields.
    (dolist (i '(0 1 2 3))
      (should (= (length (nth i t0)) (length (nth i t1)))))))

;;; Body and helper templates

(ert-deftest nelisp-eln-s69-genuine-body-matches ()
  (skip-unless (file-readable-p nelisp-eln-s69-test--eln))
  (let ((analysis (nelisp-eln-tail-code-analyze-multi-import-call
                   (nelisp-eln-s69-test--slice #x1150 3909) #x1150)))
    (should (eq (plist-get analysis :shape) 'lambda-form))
    (should (eq (plist-get analysis :proof) :multi-import-call))
    (should (= (length (plist-get analysis :imports)) 21))
    (should (cl-every (lambda (i) (= (plist-get i :got-vaddr) #x4fd8))
                      (plist-get analysis :imports)))
    (should (cl-every (lambda (d) (= (plist-get d :got-vaddr) #x4fc8))
                      (plist-get analysis :data-relocations)))
    (should (= (plist-get analysis :symbols-with-pos-got) #x4fa8))
    (should (plist-get analysis :helper))))

(ert-deftest nelisp-eln-s69-tampered-body-byte-rejected ()
  "Every fixed byte of the body, flipped one at a time, is rejected."
  (skip-unless (file-readable-p nelisp-eln-s69-test--eln))
  (let* ((shape (nelisp-eln-s69-test--shape))
         (bytes (nelisp-eln-s69-test--slice #x1150 3909))
         (template (plist-get shape :template))
         (holes (* 4 (length (plist-get shape :gots))))
         (checked 0))
    (should (= (length template) 3909))
    (dotimes (i 3909)
      (when (aref template i)
        (let ((tampered (copy-sequence bytes)))
          (aset tampered i (logxor (aref tampered i) 1))
          (should-not (nelisp-eln-tail-code-analyze-multi-import-call
                       tampered #x1150))
          (setq checked (1+ checked)))))
    (should (= checked (- 3909 holes)))))

(ert-deftest nelisp-eln-s69-call-slots-are-fixed-bytes ()
  "Every indirect call displacement in the body is a fixed template byte,
so an unauthenticated call target is a template mismatch."
  (skip-unless (file-readable-p nelisp-eln-s69-test--eln))
  (let* ((bytes (nelisp-eln-s69-test--slice #x1150 3909))
         (slots (plist-get (nelisp-eln-s69-test--shape) :imports))
         (seen nil))
    (dotimes (i (- 3909 6))
      ;; call/jmp *disp(%r15): 41 ff (97|a7) disp32, or (57|67) disp8
      (when (and (= (aref bytes i) #x41) (= (aref bytes (1+ i)) #xff))
        (let ((slot (cond ((memq (aref bytes (+ i 2)) '(#x97 #xa7))
                           (/ (nelisp-eln-tail-code--disp32 bytes (+ i 3)) 8))
                          ((memq (aref bytes (+ i 2)) '(#x57 #x67))
                           (/ (aref bytes (+ i 3)) 8)))))
          (when slot
            (should (memq slot slots))
            (cl-pushnew slot seen)))))
    ;; 13 and 14 are only reached through the helper.
    (should (equal (sort (copy-sequence seen) #'<)
                   (sort (cl-remove-if (lambda (s) (memq s '(13 14)))
                                       (copy-sequence slots))
                         #'<)))))

(ert-deftest nelisp-eln-s69-helper-template ()
  (skip-unless (file-readable-p nelisp-eln-s69-test--eln))
  (let* ((helper (plist-get (nelisp-eln-s69-test--shape) :helper))
         (template (plist-get helper :template))
         (bytes (nelisp-eln-s69-test--slice #x1100 80)))
    (should (= (plist-get helper :back) 80))
    (should (= (length template) 80))
    (should (nelisp-eln-tail-code--match-template bytes template))
    (let ((checked 0))
      (dotimes (i 80)
        (when (aref template i)
          (let ((tampered (copy-sequence bytes)))
            (aset tampered i (logxor (aref tampered i) 1))
            (should-not (nelisp-eln-tail-code--match-template tampered template))
            (setq checked (1+ checked)))))
      (should (= checked (- 80 (* 4 (length (plist-get helper :gots)))))))))

;;; Port and constant authentication

(ert-deftest nelisp-eln-s69-port-specs-authenticate ()
  (let ((spec (cdr (assq 'lambda-form nelisp-eln-native-subr--multi-import-specs))))
    (should (= (length (plist-get spec :ports)) 21))
    (dolist (port (plist-get spec :ports))
      (when (eq (nth 1 port) 'fixed)
        (should (plist-get (nelisp-eln-native-subr--multi-port-spec
                            "ba35c031" port)
                           :implementation))))
    ;; Wrong arity or convention for a new service slot, or a slot that is
    ;; no service at all, is refused.
    (dolist (port '((1263 fixed 2 (lisp lisp) lisp)
                    (1323 fixed 2 (lisp lisp) lisp)
                    (1392 fixed 2 (lisp lisp) lisp)
                    (951 fixed 1 (lisp) void)
                    (1218 fixed 3 (lisp lisp lisp) lisp)
                    (1218 many 2 (lisp lisp) lisp)
                    (1264 fixed 3 (lisp lisp lisp) lisp)))
      (should-error (nelisp-eln-native-subr--multi-port-spec "ba35c031" port)
                    :type 'nelisp-eln-native-subr-error))
    ;; MANY rows: only the authenticated argument counts.
    (dolist (port '((946 many 3 (lisp lisp lisp) lisp)
                    (1117 many 3 (lisp lisp lisp) lisp)
                    (1236 many (2 4) (lisp lisp lisp lisp) lisp)
                    (1117 many (2 3) (lisp lisp lisp) lisp)))
      (should-error (nelisp-eln-native-subr--multi-port-spec "ba35c031" port)
                    :type 'nelisp-eln-native-subr-error))))

(ert-deftest nelisp-eln-s69-new-services-behave-like-gnu ()
  (should (equal (nelisp-eln-runtime-services-fmember 'b '(a b c)) '(b c)))
  (should-not (nelisp-eln-runtime-services-fmember 'z '(a b c)))
  (let ((h (make-hash-table :test 'equal)))
    (puthash "k" 1 h)
    (should (= (nelisp-eln-runtime-services-fgethash "k" h nil) 1))
    (should (eq (nelisp-eln-runtime-services-fgethash "q" h 'd) 'd)))
  (let ((v (vector 1 2 3)))
    (should (= (nelisp-eln-runtime-services-faset v 1 9) 9))
    (should (equal v [1 9 3])))
  (should (eq (nelisp-eln-runtime-services-ftype-of 1) 'integer))
  (should (eq (car (should-error
                    (nelisp-eln-runtime-services-fsignal 'wrong-type-argument
                                                         '(listp 1))))
              'wrong-type-argument))
  (should-not (nelisp-eln-runtime-services-validate-descriptors)))

(ert-deftest nelisp-eln-s69-string-constant-identity ()
  (should (nelisp-eln-native-subr--multi-constant-matches-p "a %S" "a %S"))
  (dolist (actual '("a %s" "" nil a 1))
    (should-not (nelisp-eln-native-subr--multi-constant-matches-p
                 actual "a %S"))))

;;; Registration code (wide `gnu-require-subr')

(defun nelisp-eln-s69-test--top ()
  (nelisp-eln-s69-test--slice nelisp-eln-s69-test--top-vaddr 102))

(ert-deftest nelisp-eln-s69-registration-code-decodes ()
  (skip-unless (file-readable-p nelisp-eln-s69-test--eln))
  (let ((top (nelisp-eln-s69-test--top)))
    (should (nelisp-eln-registration--match-holed-template
             top nelisp-eln-registration--gnu-require-subr-wide-template
             nelisp-eln-registration--gnu-require-subr-wide-holes))
    ;; max 2 (ecx), min 1 (edx).
    (should (equal (list (nelisp-eln-registration--gnu-arity top 61)
                         (nelisp-eln-registration--gnu-arity top 70))
                   '(2 1)))
    ;; Feval lexenv 46? no: rsi=0x170 (46), rdi=0x160 (44); type 0x168 (45).
    (should (equal (mapcar (lambda (o)
                             (nelisp-eln-registration--d-reloc-index32 top o))
                           '(27 34 56))
                   '(46 44 45)))
    ;; The other require templates do not admit it (different length).
    (dolist (template (list nelisp-eln-registration--gnu-require-subr-template
                            nelisp-eln-registration--gnu-require-subr-opt-template))
      (should-not (= (length template) (length top))))))

(ert-deftest nelisp-eln-s69-tampered-registration-code-rejected ()
  "Every fixed byte of top_level_run, including both call slots, flipped
one at a time, leaves the template unmatched."
  (skip-unless (file-readable-p nelisp-eln-s69-test--eln))
  (let* ((top (nelisp-eln-s69-test--top))
         (holes nelisp-eln-registration--gnu-require-subr-wide-holes)
         (checked 0))
    (dotimes (i (length top))
      (unless (nelisp-eln-registration--offset-holed-p i holes)
        (let ((tampered (copy-sequence top)))
          (aset tampered i (logxor (aref tampered i) 1))
          (should-not (nelisp-eln-registration--match-holed-template
                       tampered
                       nelisp-eln-registration--gnu-require-subr-wide-template
                       holes))
          (setq checked (1+ checked)))))
    (should (= checked (- 102 (* 4 (length holes)))))
    ;; The two call slots: 947 `Feval' and 1030 `register_subr'.
    (should (equal (mapcar (lambda (o) (+ (aref top o) (* 256 (aref top (1+ o)))))
                           '(42 91))
                   '(#x1d98 #x2030)))))

;;; End-to-end on a standalone binary (optional)

(defconst nelisp-eln-s69-test--root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name
                             default-directory))))
  "Repository root, captured while this file loads.")

(defun nelisp-eln-s69-test--load-reason (offset expected new)
  "Load a copy of the artifact with file byte OFFSET (EXPECTED) set to NEW
on the standalone binary and return the printed failure text."
  (let* ((dir (make-temp-file "s69-tamper" t))
         (eln (expand-file-name "gnu-byte-compile-lambda.eln" dir))
         (probe (expand-file-name "probe.el" dir)))
    (unwind-protect
        (progn
          (copy-file nelisp-eln-s69-test--eln eln)
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert-file-contents-literally eln)
            (should (= (char-after (1+ offset)) expected))
            (goto-char (1+ offset))
            (delete-char 1)
            (insert new)
            (let ((coding-system-for-write 'binary))
              (write-region nil nil eln)))
          (with-temp-file probe
            (insert (format "(condition-case err (progn (load %S nil t t) (princ \"S69_LOADED\\n\")) (error (princ (format \"S69_REJECTED %%S\\n\" err))))\n" eln)))
          (with-temp-buffer
            (let ((default-directory nelisp-eln-s69-test--root))
              (call-process
               "sh" nil t nil "-c"
               (format ". test/lib/nelisp-boot-args.sh; nl_cold_image_setup %s || exit 1; timeout 600 %s ${NL_COLD_IMAGE_PATH:+--cold-load-from \"$NL_COLD_IMAGE_PATH\"} -L lisp -L src -L packages/nl-ffi/src --load lisp/nelisp-eln-native-subr.el --load lisp/nelisp-eln-registration.el --load %s 2>&1"
                       (shell-quote-argument (getenv "NELISP_S69_BIN"))
                       (shell-quote-argument (getenv "NELISP_S69_BIN"))
                       (shell-quote-argument probe))))
            (buffer-string)))
      (delete-directory dir t))))

(defmacro nelisp-eln-s69-test--e2e (name doc offset expected new reason)
  `(ert-deftest ,name ()
     ,doc
     (skip-unless (and (getenv "NELISP_S69_BIN")
                       (file-executable-p (getenv "NELISP_S69_BIN"))
                       (file-readable-p nelisp-eln-s69-test--eln)))
     (let ((out (nelisp-eln-s69-test--load-reason ,offset ,expected ,new)))
       (should-not (string-match-p "S69_LOADED" out))
       (should (string-match-p ,reason out)))))

(nelisp-eln-s69-test--e2e
 nelisp-eln-s69-e2e-tampered-init-got-displacement-rejected
 "The shifted `.init' GOT displacement, one bit."
 #x1007 #xd5 #xd4 "preopen-executable-region-not-admitted")

(nelisp-eln-s69-test--e2e
 nelisp-eln-s69-e2e-tampered-body-rejected
 "The body's `sub $0x248,%rsp' immediate."
 #x1160 #x48 #x50 "leaf-instructions-not-admitted")

(nelisp-eln-s69-test--e2e
 nelisp-eln-s69-e2e-unauthenticated-call-slot-rejected
 "The body's first import call goes through slot 1355, not 1354."
 #x1176 #x50 #x58 "leaf-instructions-not-admitted")

(nelisp-eln-s69-test--e2e
 nelisp-eln-s69-e2e-tampered-helper-rejected
 "The helper's counter increment immediate."
 #x1108 #x01 #x02 "helper-instructions")

(nelisp-eln-s69-test--e2e
 nelisp-eln-s69-e2e-feval-slot-rejected
 "The `Feval' call in top_level_run goes through a different slot."
 #x20ca #x98 #x90 "top-level-instructions-not-admitted")

(nelisp-eln-s69-test--e2e
 nelisp-eln-s69-e2e-wrong-type-slot-rejected
 "The registered type slot is redirected onto the Feval lexenv slot."
 #x20d8 #x68 #x60 "top-level-instructions-not-admitted")

(provide 'nelisp-eln-s69-lambda-admission-test)

;;; nelisp-eln-s69-lambda-admission-test.el ends here
