;;; nelisp-eln-s67-convert-function-admission-test.el --- S6.7 cconv--convert-function admission -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Focused host tests for admitting the genuine GNU 31.1 (ba35c031)
;; artifact of vendor cconv.el `cconv--convert-function'
;; (tools/ai/eln-progress.org S6.7):
;;
;;   - its top_level_run registers ONE native anonymous lambda through
;;     `Fcomp__register_lambda' (slot 1031), evaluates `(require \\='cconv)'
;;     (slot 947) and registers the `&optional' subr (slot 1030): the
;;     `gnu-lambda-call-subr' profile, role plan (lambda require register);
;;   - the 252-byte lambda and the 1594-byte body match exact templates with
;;     every import slot, d_reloc constant and the module `quitcounter'
;;     authenticated; the body calls the lambda directly through the
;;     module-local PLT, admitted only when the artifact's own `.plt' entry,
;;     `.rela.plt' JUMP_SLOT, `.dynsym' and dynamic section say exactly that;
;;   - the fixed `.plt.got'/CRT-stub templates are re-based by the layout the
;;     extra PLT entry causes, and by nothing else;
;;   - any single changed fixed byte, a lambda registered into a wrong slot
;;     or with a wrong arity, a tampered PLT/relocation, an extra
;;     registration and wrong argument counts are all rejected.
;;
;; Tests needing the genuine artifact skip when it is absent.  The
;; end-to-end checks run only when NELISP_S67_BIN names a standalone binary
;; (with its adjacent `.cold' image, if any).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-eln-registration)
(require 'nelisp-eln-emitter)

(defconst nelisp-eln-s67-test--eln
  (expand-file-name
   "~/.cache/tmp/s6-survey-lex/cconv--convert-function/overlay/eln/31.1-ba35c031/gnu-cconv--convert-function.eln")
  "The genuine artifact (sha256 2cdde607..184d0bdd).")

(defconst nelisp-eln-s67-test--bodies
  '((cconv-convert-lambda #x1110 252 (1354 1119 0) (0 16))
    (cconv-convert-function-form #x1210 1594
                                 (1335 1201 10 1354 1119 1209 1215 1301 1353
                                       7 945 0 13 14)
                                 (0 2 4 5 6 7 9 10 11 15 16 17)))
  "(SHAPE VADDR SIZE IMPORT-SLOTS DATA-SLOTS) of the two genuine bodies.")

(defconst nelisp-eln-s67-test--top-vaddr #x1850)

(defun nelisp-eln-s67-test--file-bytes ()
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally nelisp-eln-s67-test--eln)
    (buffer-string)))

(defun nelisp-eln-s67-test--slice (vaddr size)
  (substring (nelisp-eln-s67-test--file-bytes) vaddr (+ vaddr size)))

(defun nelisp-eln-s67-test--analyze (bytes vaddr)
  (nelisp-eln-tail-code-analyze-multi-import-call bytes vaddr))

(defun nelisp-eln-s67-test--patched (offset new)
  "The genuine file bytes with the byte at OFFSET replaced by NEW."
  (let ((bytes (copy-sequence (nelisp-eln-s67-test--file-bytes))))
    (aset bytes offset new)
    bytes))

;;; Body templates

(ert-deftest nelisp-eln-s67-genuine-bodies-match ()
  (skip-unless (file-readable-p nelisp-eln-s67-test--eln))
  (dolist (body nelisp-eln-s67-test--bodies)
    (pcase-let ((`(,shape ,vaddr ,size ,imports ,data) body))
      (let ((analysis (nelisp-eln-s67-test--analyze
                       (nelisp-eln-s67-test--slice vaddr size) vaddr)))
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
        (if (eq shape 'cconv-convert-function-form)
            (progn
              (should (= (plist-get analysis :module-counter-vaddr) #x4364))
              (should (= (plist-get analysis :symbols-with-pos-got) #x3fa8))
              ;; The one PLT call reaches the `.plt' entry (PLT0 + 16).
              (should (equal (plist-get analysis :plt-calls) '(#x1030))))
          (should-not (plist-get analysis :module-counter-vaddr))
          (should-not (plist-get analysis :symbols-with-pos-got))
          (should-not (plist-get analysis :plt-calls)))
        (should-not (plist-get analysis :helper))))))

(ert-deftest nelisp-eln-s67-tampered-body-byte-rejected ()
  "Every fixed byte of both bodies, flipped one at a time, is rejected."
  (skip-unless (file-readable-p nelisp-eln-s67-test--eln))
  (dolist (body nelisp-eln-s67-test--bodies)
    (pcase-let ((`(,shape ,vaddr ,size ,_ ,_) body))
      (let* ((bytes (nelisp-eln-s67-test--slice vaddr size))
             (entry (cdr (assq shape nelisp-eln-tail-code--multi-import-shapes)))
             (template (plist-get entry :template))
             (holes (+ (* 4 (length (plist-get entry :gots)))
                       (* 4 (length (plist-get entry :plt-calls)))
                       (* 4 (length (plist-get entry :module-counter)))))
             (checked 0))
        (should (= (length template) size))
        (dotimes (i size)
          (when (aref template i)
            (let ((tampered (copy-sequence bytes)))
              (aset tampered i (logxor (aref tampered i) 1))
              (should-not (nelisp-eln-s67-test--analyze tampered vaddr))
              (setq checked (1+ checked)))))
        (should (= checked (- size holes)))))))

(ert-deftest nelisp-eln-s67-plt-call-target-is-reported ()
  "A moved `call rel32' target is reported (not silently normalized): the
registration compares it with the artifact's own `.plt' entry."
  (skip-unless (file-readable-p nelisp-eln-s67-test--eln))
  (let* ((bytes (copy-sequence
                 (nelisp-eln-s67-test--slice #x1210 1594)))
         (analysis (progn (aset bytes 632 (1+ (aref bytes 632)))
                          (nelisp-eln-s67-test--analyze bytes #x1210))))
    (should (equal (plist-get analysis :plt-calls) '(#x1031)))))

(ert-deftest nelisp-eln-s67-port-specs-authenticate ()
  (dolist (shape '(cconv-convert-lambda cconv-convert-function-form))
    (let ((spec (cdr (assq shape nelisp-eln-native-subr--multi-import-specs)))
          (entry (cdr (assq shape nelisp-eln-tail-code--multi-import-shapes))))
      (should spec)
      ;; The spec's ports are the shape's imports, in the same order.
      (should (equal (mapcar #'car (plist-get spec :ports))
                     (plist-get entry :imports)))
      ;; The spec's constants are the shape's d_reloc reads, in order.
      (should (equal (mapcar #'car (plist-get spec :constants))
                     (plist-get entry :data)))
      (should (= (nelisp-eln-native-subr-multi-arity (list :shape shape)) 5))
      ;; Only fixed runtime-service ports resolve on the host; the MANY row
      ;; is a canonical builtin (checked on NeLisp).
      (dolist (port (plist-get spec :ports))
        (when (eq (nth 1 port) 'fixed)
          (should (plist-get (nelisp-eln-native-subr--multi-port-spec
                              "ba35c031" port)
                             :implementation))))))
  (should (= (nelisp-eln-native-subr-multi-min-arity
              (list :shape 'cconv-convert-function-form))
             4))
  (should (= (nelisp-eln-native-subr-multi-min-arity
              (list :shape 'cconv-convert-lambda))
             5))
  (should (nelisp-eln-native-subr-multi-swp-declared-p
           (list :shape 'cconv-convert-function-form)))
  (should-not (nelisp-eln-native-subr-multi-swp-declared-p
               (list :shape 'cconv-convert-lambda)))
  ;; The nested lambda runs inside the body's own activation: every port it
  ;; imports is one of the body's with the identical descriptor, and every
  ;; constant it reads is declared, with the same value, by the body.
  (let ((lam (cdr (assq 'cconv-convert-lambda
                        nelisp-eln-native-subr--multi-import-specs)))
        (main (cdr (assq 'cconv-convert-function-form
                         nelisp-eln-native-subr--multi-import-specs))))
    (dolist (port (plist-get lam :ports))
      (should (equal port (assq (car port) (plist-get main :ports)))))
    (dolist (constant (plist-get lam :constants))
      (should (equal constant (assq (car constant) (plist-get main :constants)))))
    ;; Fixed-arity-5 registered lambda, `&optional' body.
    (should-not (plist-get lam :min-arity))
    (should (equal (plist-get main :module-counter) "quitcounter")))
  ;; The two-argument and five-argument `Ffuncall' rows are the only ones.
  (should-error (nelisp-eln-native-subr--multi-port-spec
                 "ba35c031" '(945 many (2 5) (lisp lisp lisp lisp) lisp))
                :type 'nelisp-eln-native-subr-error))

(ert-deftest nelisp-eln-s67-runtime-services ()
  (should (eq (nelisp-eln-runtime-services-fequal '(a (b)) '(a (b))) t))
  (should-not (nelisp-eln-runtime-services-fequal '(a) '(b)))
  (should (equal (nelisp-eln-runtime-services-fcdr-safe '(1 2)) '(2)))
  (should-not (nelisp-eln-runtime-services-fcdr-safe 5))
  (dolist (row '((1201 "Fequal" 2) (1353 "Fcdr_safe" 1)))
    (let ((d (nelisp-eln-native-subr--runtime-services-descriptor (car row))))
      (should d)
      (should (equal (plist-get d :symbol) (nth 1 row)))
      (should (eq (plist-get d :convention) 'fixed))
      (should (= (plist-get d :arity) (nth 2 row)))
      (should (eq (plist-get d :status) 'supported)))))

(ert-deftest nelisp-eln-s67-runtime-service-descriptors-validate ()
  "The new descriptors agree with the authenticated freloc table."
  (skip-unless (file-readable-p nelisp-eln-runtime-services-freloc-tsv-file))
  (should-not (nelisp-eln-runtime-services-validate-descriptors)))

;;; Registration code (`gnu-lambda-call-subr')

(defun nelisp-eln-s67-test--top ()
  (nelisp-eln-s67-test--slice nelisp-eln-s67-test--top-vaddr 139))

(ert-deftest nelisp-eln-s67-registration-code-decodes ()
  (skip-unless (file-readable-p nelisp-eln-s67-test--eln))
  (let ((top (nelisp-eln-s67-test--top)))
    (should (nelisp-eln-registration--match-holed-template
             top nelisp-eln-registration--gnu-lambda-call-subr-template
             nelisp-eln-registration--gnu-lambda-call-subr-holes))
    ;; Lambda arity 5/5, subr arity 5 and minimum 4.
    (should (equal (mapcar (lambda (o) (nelisp-eln-registration--gnu-arity top o))
                           '(3 8 100 113))
                   '(5 5 5 4)))
    ;; Lambda d_reloc index 8.
    (should (= (nelisp-eln-registration--gnu-fixnum-immediate top 62) 8))
    ;; Lambda type slot 13, subr type slot 14, Feval lexenv 15 and form 12.
    (should (equal (mapcar (lambda (o) (nelisp-eln-registration--d-reloc-index top o))
                           '(52 98 77 82))
                   '(13 14 15 12)))
    ;; The earlier profiles' templates are other lengths.
    (dolist (template (list nelisp-eln-registration--gnu-require-subr-template
                            nelisp-eln-registration--gnu-lambda-require-subr-template
                            nelisp-eln-registration--gnu-lambdas-require-subr-template
                            nelisp-eln-registration--gnu-eval-subr-pair-template
                            nelisp-eln-registration--gnu-eval-subr-template))
      (should-not (= (length template) (length top))))))

(ert-deftest nelisp-eln-s67-tampered-registration-code-rejected ()
  "Every fixed byte of top_level_run, including each import slot
displacement, flipped one at a time, leaves the template unmatched."
  (skip-unless (file-readable-p nelisp-eln-s67-test--eln))
  (let* ((top (nelisp-eln-s67-test--top))
         (holes nelisp-eln-registration--gnu-lambda-call-subr-holes)
         (checked 0))
    (dotimes (i (length top))
      (unless (nelisp-eln-registration--offset-holed-p i holes)
        (let ((tampered (copy-sequence top)))
          (aset tampered i (logxor (aref tampered i) 1))
          (should-not (nelisp-eln-registration--match-holed-template
                       tampered
                       nelisp-eln-registration--gnu-lambda-call-subr-template
                       holes))
          (setq checked (1+ checked)))))
    (should (= checked (- 139 (apply #'+ (mapcar (lambda (h) (- (cdr h) (car h)))
                                                 holes)))))
    ;; The three call slots: 1031 (lambda), 947 (Feval), 1030 (subr).
    (should (equal (mapcar (lambda (o) (+ (aref top o) (* 256 (aref top (1+ o)))))
                           '(69 86 124))
                   '(#x2038 #x1d98 #x2030)))))

(defun nelisp-eln-s67-test--metadata ()
  (let ((data (make-vector 19 nil)))
    (aset data 8 "#$")
    (aset data 15 t)
    (aset data 12 '(require 'cconv))
    (aset data 13 '(function (t t t t t) t))
    (aset data 14 '(function (t t t t &optional t) cons))
    (list :abi-hash "ba35c031"
          :data-relocations data
          :ephemeral-data-relocations
          (vector 5 (concat nelisp-eln-registration--lambda-c-name-prefix "0")
                  '(0 nil nil) 4 'cconv--convert-function
                  (nelisp-eln-emitter--symbol-name 'cconv--convert-function)
                  '(1 nil nil))
          :function-docs [nil nil]
          :d-reloc-size 152 :d-reloc-eph-size 56)))

(defconst nelisp-eln-s67-test--extra
  '(:type-index 14 :lexenv-index 15 :form-index 12 :min-arity 4
    :lambda-type-index 13 :lambda-idx1 8 :lambda-arity 5))

(defun nelisp-eln-s67-test--count (metadata extra)
  (nelisp-eln-registration--metadata-data-count
   metadata 'gnu-lambda-call-subr 5 extra))

(defun nelisp-eln-s67-test--bad-count (metadata extra)
  (cadr (should-error (nelisp-eln-s67-test--count metadata extra)
                      :type 'nelisp-eln-registration-error)))

(ert-deftest nelisp-eln-s67-metadata-envelope ()
  (let ((metadata (nelisp-eln-s67-test--metadata)))
    (should (= (nelisp-eln-s67-test--count metadata nelisp-eln-s67-test--extra)
               19))
    (should (eq (nelisp-eln-registration--require-effect metadata 12 15)
                'cconv))
    ;; A lambda registered over a slot that is not a "#$" placeholder, out of
    ;; range, the Feval form/lexenv slots, the type slots, or with an arity
    ;; its type does not have, and a wrong subr type or minimum arity.
    (dolist (edit '((:lambda-idx1 . 9) (:lambda-idx1 . 99) (:lambda-idx1 . 12)
                    (:lambda-idx1 . 15) (:lambda-idx1 . 13) (:lambda-idx1 . 14)
                    (:lambda-arity . 4) (:lambda-type-index . 14)
                    (:lambda-type-index . 0) (:lambda-type-index . 12)
                    (:type-index . 13) (:type-index . 15) (:min-arity . 3)
                    (:min-arity . 5)))
      (should (eq (nelisp-eln-s67-test--bad-count
                   metadata (plist-put (copy-sequence nelisp-eln-s67-test--extra)
                                       (car edit) (cdr edit)))
                  'metadata-outside-emitter-slice)))
    ;; Too few documentation descriptors.
    (should (eq (nelisp-eln-s67-test--bad-count
                 (plist-put (copy-sequence metadata) :function-docs [nil])
                 nelisp-eln-s67-test--extra)
                'metadata-outside-emitter-slice))
    ;; Wrong lambda C names, descriptors or ephemeral shape.
    (dolist (edit '((0 . 4) (0 . 6) (1 . "F616e6f6e796d6f75732d6c616d626461_anonymous_lambda_1")
                    (1 . "some_other_function") (2 . (0 t nil)) (3 . 5)
                    (6 . (0 nil nil))))
      (let ((eph (copy-sequence (plist-get metadata :ephemeral-data-relocations))))
        (aset eph (car edit) (cdr edit))
        (should (eq (nelisp-eln-s67-test--bad-count
                     (plist-put (copy-sequence metadata)
                                :ephemeral-data-relocations eph)
                     nelisp-eln-s67-test--extra)
                    'metadata-outside-emitter-slice))))
    (should (eq (nelisp-eln-s67-test--bad-count
                 (plist-put (copy-sequence metadata) :d-reloc-eph-size 64)
                 nelisp-eln-s67-test--extra)
                'metadata-outside-emitter-slice))))

;;; `.plt' / `.rela.plt' / layout

(ert-deftest nelisp-eln-s67-plt-target-name ()
  (skip-unless (file-readable-p nelisp-eln-s67-test--eln))
  (let ((bytes (nelisp-eln-s67-test--file-bytes)))
    (should (equal (nelisp-eln-registration--plt-target-name bytes)
                   "F616e6f6e796d6f75732d6c616d626461_anonymous_lambda_0"))
    ;; Relocation offset, type, symbol index, addend; the fourth GOT word;
    ;; the PLT entry's lazy-binding target; the dynamic tags.
    (dolist (offset '(#x740 #x748 #x74c #x750 #x3000))
      (should-not (nelisp-eln-registration--plt-target-name
                   (nelisp-eln-s67-test--patched
                    offset (logxor (aref bytes offset) 1)))))
    ;; The symbol index pointing at another function, and at an undefined
    ;; symbol, is refused.
    (dolist (index '(9 12 1))
      (should-not (nelisp-eln-registration--plt-target-name
                   (nelisp-eln-s67-test--patched #x74c index))))
    ))

(ert-deftest nelisp-eln-s67-layout-adjustments ()
  (skip-unless (file-readable-p nelisp-eln-s67-test--eln))
  (let ((bytes (nelisp-eln-s67-test--file-bytes)))
    (should (= (nelisp-eln-registration--layout-shift-in-bytes bytes) 0))
    (should (equal (nelisp-eln-registration--layout-adjustments bytes 0)
                   '(16 32 t)))
    ;; A layout whose relocation does not authenticate is refused.
    (should-not (nelisp-eln-registration--layout-adjustments
                 (nelisp-eln-s67-test--patched #x748 6) 0))
    (let ((templates (nelisp-eln-registration--fixed-templates 0 bytes)))
      (should (= (length (nth 1 templates)) 32))
      (should (equal (nth 1 templates)
                     (substring bytes #x1020 #x1040)))
      (should (equal (nth 2 templates) (substring bytes #x1040 #x1048)))
      (should (nelisp-eln-registration--match-holed-template
               (substring bytes #x1050 (+ #x1050 192)) (nth 3 templates)
               nelisp-eln-registration--crt-stub-holes)))
    ;; The genuine file passes the pre-open validator; a changed PLT entry,
    ;; `.plt.got' byte or CRT stub byte, and a changed relocation, do not.
    (should-not (nelisp-eln-registration--validate-preopen bytes))
    (dolist (offset '(#x1031 #x1036 #x103b #x103c #x1040 #x1050 #x1063 #x748))
      (should-error (nelisp-eln-registration--validate-preopen
                     (nelisp-eln-s67-test--patched
                      offset (logxor (aref bytes offset) 1)))
                    :type 'nelisp-eln-registration-error))
    ;; A plain 16-byte `.plt' artifact keeps the original templates.
    (let ((templates (nelisp-eln-registration--fixed-templates 0)))
      (should (= (length (nth 1 templates)) 16))
      (should (equal (nth 2 templates)
                     nelisp-eln-registration--plt-got-template)))))

;;; Role sequence: nothing beyond the admitted calls

(defun nelisp-eln-s67-test--owner ()
  (let ((owner (make-vector nelisp-eln-registration--owner-size nil)))
    (aset owner 18 (list :role-sequence '(lambda require register)
                         :lambda-call t
                         :lambdas (list (list :idx 8 :c-name "a" :arity 5
                                              :type-index 13))))
    owner))

(ert-deftest nelisp-eln-s67-role-sequence ()
  (let ((owner (nelisp-eln-s67-test--owner)) (roles nil))
    (dolist (n '(1 2 3 4 5))
      (let ((nelisp-eln-registration--call-index n))
        (push (nelisp-eln-registration--call-role owner) roles)))
    (should (equal (nreverse roles) '(lambda require register nil nil)))
    (let ((nelisp-eln-registration--call-index 1))
      (should (= (nelisp-eln-registration--lambda-ordinal owner) 1)))
    (let ((nelisp-eln-registration--call-index 3))
      (should (= (nelisp-eln-registration--register-ordinal owner) 1)))))

(ert-deftest nelisp-eln-s67-extra-registration-rejected ()
  "A call past the third admitted one (an extra registration) is refused,
and the failure is sticky."
  (let* ((owner (nelisp-eln-s67-test--owner))
         (nelisp-eln-registration--owners (list owner))
         (nelisp-eln-registration--active-owner owner)
         (nelisp-eln-registration--call-index 3))
    (should-error (nelisp-eln-registration--callback 0)
                  :type 'nelisp-eln-registration-error)
    (should (eq (cadr nelisp-eln-registration--last-callback-error)
                'callback-sequence-not-admitted))
    (should (eq (cadr (car nelisp-eln-registration--callback-failures))
                'callback-sequence-not-admitted)))
  (setq nelisp-eln-registration--callback-failures nil))

(ert-deftest nelisp-eln-s67-unadmitted-lambda-callback-rejected ()
  "The `register_lambda' callback with no owner, an ordinal other than 1,
or an owner that does not expect it is refused before anything is built."
  (let ((nelisp-eln-registration--active-owner nil))
    (cl-letf (((symbol-function 'nelisp-eln-abi-read-word)
               (lambda (_address _offset) 0)))
      (should (eq (cadr (should-error
                         (nelisp-eln-registration--register-lambda-call-callback
                          #x10000 1)
                         :type 'nelisp-eln-registration-error))
                  'callback-arguments-not-admitted))))
  (let* ((owner (nelisp-eln-s67-test--owner))
         (nelisp-eln-registration--active-owner owner))
    (cl-letf (((symbol-function 'nelisp-eln-abi-read-word)
               (lambda (_address _offset) 0)))
      (dolist (ordinal '(0 2 3))
        (should (eq (cadr (should-error
                           (nelisp-eln-registration--register-lambda-call-callback
                            #x10000 ordinal)
                           :type 'nelisp-eln-registration-error))
                    'callback-arguments-not-admitted))))))

;;; End-to-end on a standalone binary (optional)

(defconst nelisp-eln-s67-test--root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name
                             default-directory))))
  "Repository root, captured while this file loads.")

(defun nelisp-eln-s67-test--bin ()
  (let ((bin (getenv "NELISP_S67_BIN")))
    (and bin (file-executable-p bin) bin)))

(defvar nelisp-eln-s67-test--wrapper nil)

(defun nelisp-eln-s67-test--normal-load-wrapper (dir)
  "Return the normal-load wrapper generated the way the S6 harness does."
  (or (and nelisp-eln-s67-test--wrapper
           (file-readable-p nelisp-eln-s67-test--wrapper)
           nelisp-eln-s67-test--wrapper)
      (let ((wrapper (expand-file-name "normal-load-wrapper.el" dir))
            (default-directory nelisp-eln-s67-test--root))
        (with-temp-buffer
          (should (eql 0 (call-process
                          (or (getenv "EMACS") "emacs") nil t nil
                          "--batch" "-Q" "-L" "scripts" "-L" "lisp" "--eval"
                          "(progn (defvar nelisp-standalone--repo-root (file-name-as-directory default-directory)) (dolist (name (list \"nelisp-standalone--core-bytecode-src\" \"nelisp-standalone--after-load-runtime-src\")) (with-temp-buffer (insert-file-contents \"scripts/nelisp-standalone-build.el\") (goto-char (point-min)) (unless (search-forward (concat \"(defun \" name) nil t) (error \"source generator not found: %s\" name)) (goto-char (match-beginning 0)) (eval (read (current-buffer))))) (princ (nelisp-standalone--after-load-runtime-src)))")))
          (write-region (point-min) (point-max) wrapper nil 'silent))
        (setq nelisp-eln-s67-test--wrapper wrapper))))

(defun nelisp-eln-s67-test--run-probe (eln probe-body)
  "Run PROBE-BODY, a Lisp form string, on the standalone binary with ELN
named by the variable `eln'; return the standard output."
  (let* ((dir (make-temp-file "s67-probe" t))
         (probe (expand-file-name "probe.el" dir))
         (bin (nelisp-eln-s67-test--bin))
         (cold (concat bin ".cold"))
         (default-directory nelisp-eln-s67-test--root))
    (unwind-protect
        (progn
          (let ((wrapper (nelisp-eln-s67-test--normal-load-wrapper dir)))
            (with-temp-file probe
              (insert (format "(setq eln %S)\n" eln) probe-body "\n"))
            (with-temp-buffer
              (apply #'call-process
                     bin nil (list t nil) nil
                     (append
                      (and (file-readable-p cold) (list "--cold-load-from" cold))
                      (list "-L" "lisp" "-L" "src" "-L" "packages/nl-ffi/src"
                            "--load" "lisp/nelisp-eln-native-subr.el"
                            "--load" "lisp/nelisp-eln-registration.el"
                            "--load" wrapper "--load" probe)))
              (buffer-string))))
      (delete-directory dir t))))

(defconst nelisp-eln-s67-test--load-probe
  "(condition-case e (progn (nelisp-eln-registration-load eln) (princ \"LOADED\\n\")) (error (princ (format \"REJECT %S %S\\n\" (car e) (cadr e)))))")

(defun nelisp-eln-s67-test--tampered-copy (dir offset expected new)
  (let ((eln (expand-file-name "gnu-cconv--convert-function.eln" dir)))
    (copy-file nelisp-eln-s67-test--eln eln)
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally eln)
      (should (= (char-after (1+ offset)) expected))
      (goto-char (1+ offset))
      (delete-char 1)
      (insert new)
      (let ((coding-system-for-write 'binary))
        (write-region nil nil eln)))
    eln))

(defun nelisp-eln-s67-test--run-tampered (offset expected new reason)
  "Load a copy with file byte OFFSET (EXPECTED) set to NEW on the standalone
binary and require the registration to be rejected naming REASON."
  (let ((dir (make-temp-file "s67-tamper" t)))
    (unwind-protect
        (let ((out (nelisp-eln-s67-test--run-probe
                    (nelisp-eln-s67-test--tampered-copy
                     dir offset expected new)
                    nelisp-eln-s67-test--load-probe)))
          (should (string-match-p "^REJECT " out))
          (should (string-match-p reason out)))
      (delete-directory dir t))))

(defmacro nelisp-eln-s67-test--e2e (name doc offset expected new reason)
  `(ert-deftest ,name ()
     ,doc
     (skip-unless (and (nelisp-eln-s67-test--bin)
                       (file-readable-p nelisp-eln-s67-test--eln)))
     (nelisp-eln-s67-test--run-tampered ,offset ,expected ,new ,reason)))

(ert-deftest nelisp-eln-s67-e2e-genuine-loads-and-runs ()
  "The genuine artifact registers, has arity (4 . 5), publishes the lambda
under no name, refuses wrong argument counts and computes GNU's results."
  (skip-unless (and (nelisp-eln-s67-test--bin)
                    (file-readable-p nelisp-eln-s67-test--eln)))
  (let ((out (nelisp-eln-s67-test--run-probe
              nelisp-eln-s67-test--eln
              "(condition-case e
  (progn
    (defvar cconv-freevars-alist nil)
    (defvar cconv-var-classification nil)
    (defvar cconv--dynbound-variables nil)
    (nelisp-eln-registration-load eln)
    (princ (format \"ARITY %S\\n\" (func-arity (symbol-function 'cconv--convert-function))))
    (princ (format \"SUBRP %S\\n\" (subrp (symbol-function 'cconv--convert-function))))
    (princ (format \"UNBOUND %S\\n\" (fboundp (intern \"F616e6f6e796d6f75732d6c616d626461_anonymous_lambda_0\"))))
    (let* ((owner (car nelisp-eln-registration--owners))
           (callables (plist-get (aref owner 18) :lambda-callables)))
      (princ (format \"LAMBDAS %S\\n\" (length callables))))
    (princ (format \"R1 %S\\n\"
                   (let ((cconv-freevars-alist '(((x)))))
                     (cconv--convert-function '(a) '(x) nil nil))))
    (dolist (args '((1 2 3) (1 2 3 4 5 6) ()))
      (princ (format \"WRONG %S\\n\"
                     (condition-case e2 (apply 'cconv--convert-function args)
                       (wrong-number-of-arguments 'wrong-number-of-arguments)
                       (error (car e2)))))))
  (error (princ (format \"PROBE-ERROR %S\\n\" e))))")))
    (should (string-match-p "^ARITY (4 \\. 5)$" out))
    (should (string-match-p "^SUBRP t$" out))
    (should (string-match-p "^UNBOUND nil$" out))
    (should (string-match-p "^LAMBDAS 1$" out))
    (should (string-match-p "^R1 #'(lambda (a) x)$" out))
    (should-not (string-match-p "PROBE-ERROR" out))
    (should (= 3 (with-temp-buffer
                   (insert out)
                   (goto-char (point-min))
                   (let ((n 0))
                     (while (re-search-forward
                             "^WRONG wrong-number-of-arguments$" nil t)
                       (setq n (1+ n)))
                     n))))))

(nelisp-eln-s67-test--e2e
 nelisp-eln-s67-e2e-tampered-lambda-body-rejected
 "The lambda's first `push %r15' -> `rex mov'."
 #x1110 #x41 #x40 "leaf-instructions-not-admitted")

(nelisp-eln-s67-test--e2e
 nelisp-eln-s67-e2e-tampered-lambda-call-target-rejected
 "The lambda's first `call *0x2a50(%rbx)' (slot 1354) redirected to slot
1355: an unauthenticated call target inside the lambda."
 #x1139 #x50 #x58 "leaf-instructions-not-admitted")

(nelisp-eln-s67-test--e2e
 nelisp-eln-s67-e2e-tampered-main-body-rejected
 "The body's first `push %r15' -> `rex mov'."
 #x1210 #x41 #x40 "leaf-instructions-not-admitted")

(nelisp-eln-s67-test--e2e
 nelisp-eln-s67-e2e-tampered-main-call-target-rejected
 "The body's `Fassq' call (slot 1215) redirected to slot 1216."
 #x1419 #xf8 #x00 "leaf-instructions-not-admitted")

(nelisp-eln-s67-test--e2e
 nelisp-eln-s67-e2e-tampered-plt-entry-rejected
 "The `.plt' lazy-binding entry's `jmp *GOT' opcode changed."
 #x1031 #x25 #x24 "preopen-executable-region-not-admitted")

(nelisp-eln-s67-test--e2e
 nelisp-eln-s67-e2e-tampered-rela-plt-type-rejected
 "The `.rela.plt' entry retyped from JUMP_SLOT to GLOB_DAT."
 #x748 #x07 #x06 "preopen-executable-region-not-admitted")

(nelisp-eln-s67-test--e2e
 nelisp-eln-s67-e2e-rela-plt-symbol-rejected
 "The `.rela.plt' entry's symbol index moved from the lambda (14) to the
subr body (9): a PLT call that would run another function."
 #x74c #x0e #x09 "preopen-executable-region-not-admitted")

(nelisp-eln-s67-test--e2e
 nelisp-eln-s67-e2e-tampered-plt-padding-rejected
 "The alignment padding after the lambda."
 #x120c #x0f #x0e "executable-region-not-admitted")

(nelisp-eln-s67-test--e2e
 nelisp-eln-s67-e2e-lambda-via-unauthenticated-slot-rejected
 "The `register_lambda' call goes through slot 1032, not 1031."
 #x1895 #x38 #x40 "top-level-instructions-not-admitted")

(nelisp-eln-s67-test--e2e
 nelisp-eln-s67-e2e-lambda-into-form-slot-rejected
 "The lambda registered over d_reloc[12] (the require form)."
 #x188e #x22 #x32 "top-level-instructions-not-admitted")

(nelisp-eln-s67-test--e2e
 nelisp-eln-s67-e2e-lambda-into-non-placeholder-slot-rejected
 "The lambda registered over d_reloc[10], which holds a symbol."
 #x188e #x22 #x2a "metadata-outside-emitter-slice")

(nelisp-eln-s67-test--e2e
 nelisp-eln-s67-e2e-lambda-arity-mismatch-rejected
 "The lambda's maximum arity raised to 6 while its minimum stays 5."
 #x1853 #x16 #x1a "top-level-instructions-not-admitted")

(nelisp-eln-s67-test--e2e
 nelisp-eln-s67-e2e-subr-minimum-arity-rejected
 "The subr's minimum arity raised to its maximum: no longer `&optional'."
 #x18c1 #x12 #x16 "top-level-instructions-not-admitted")

(provide 'nelisp-eln-s67-convert-function-admission-test)

;;; nelisp-eln-s67-convert-function-admission-test.el ends here
