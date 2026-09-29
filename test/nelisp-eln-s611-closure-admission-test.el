;;; nelisp-eln-s611-closure-admission-test.el --- S6.11 byte-compile-make-closure admission -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Focused host tests for admitting the genuine GNU 31.1 (ba35c031)
;; artifact of vendor bytecomp.el `byte-compile-make-closure'
;; (tools/ai/eln-progress.org S6.11):
;;
;;   - its top_level_run registers THREE native anonymous lambdas through
;;     `Fcomp__register_lambda' (slot 1031), then evaluates
;;     `(require \\='bytecomp)' (slot 947) and registers the subr (slot
;;     1030): the `gnu-lambdas-require-subr' profile, role plan (lambda
;;     lambda lambda require register);
;;   - the four bodies (71-byte `lambda-intern-format', 27-byte
;;     `lambda-aref-form', 43-byte `lambda-cons-form', 1667-byte
;;     `make-closure-form') match exact templates with every import slot and
;;     d_reloc constant authenticated;
;;   - each registered lambda becomes a callable native subr built from its
;;     own proof and lease; the four bodies share one link table through one
;;     port numbering, a slot two bodies import being admitted only with the
;;     same authenticated descriptor;
;;   - any single changed fixed byte, a lambda registered into a wrong slot,
;;     a call through an unauthenticated slot, an extra registration and a
;;     registered lambda called with the wrong arity are all rejected.
;;
;; Tests needing the genuine artifact skip when it is absent.  The
;; end-to-end rejections run only when NELISP_S611_BIN names a standalone
;; binary (with its adjacent `.cold' image, if any).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-eln-registration)
(require 'nelisp-eln-emitter)

(defconst nelisp-eln-s611-test--eln
  (expand-file-name
   "~/.cache/tmp/s6-survey-lex/byte-compile-make-closure/overlay/eln/31.1-ba35c031/gnu-byte-compile-make-closure.eln")
  "The genuine artifact (sha256 5a0d9680..418bcb).")

(defconst nelisp-eln-s611-test--bodies
  '((lambda-intern-format #x1100 71 (704 1006) (2))
    (lambda-aref-form #x1150 27 (1324) nil)
    (lambda-cons-form #x1170 43 (1119) (4))
    (make-closure-form #x11a0 1667
                       (1335 10 1220 1221 1119 1250 945 1318 1364 1300 1195
                             1113 1324 1234 946 1236 0)
                       (6 10 11 13 14 15 17 18 19 21 24 25 26 27 29 30 31
                          32 33 39)))
  "(SHAPE VADDR SIZE IMPORT-SLOTS DATA-SLOTS) of the four genuine bodies.")

(defconst nelisp-eln-s611-test--top-vaddr #x1830)

(defun nelisp-eln-s611-test--file-bytes ()
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally nelisp-eln-s611-test--eln)
    (buffer-string)))

(defun nelisp-eln-s611-test--slice (vaddr size)
  (substring (nelisp-eln-s611-test--file-bytes) vaddr (+ vaddr size)))

(defun nelisp-eln-s611-test--analyze (bytes vaddr)
  (nelisp-eln-tail-code-analyze-multi-import-call bytes vaddr))

;;; Body templates

(ert-deftest nelisp-eln-s611-genuine-bodies-match ()
  (skip-unless (file-readable-p nelisp-eln-s611-test--eln))
  (dolist (body nelisp-eln-s611-test--bodies)
    (pcase-let ((`(,shape ,vaddr ,size ,imports ,data) body))
      (let ((analysis (nelisp-eln-s611-test--analyze
                       (nelisp-eln-s611-test--slice vaddr size) vaddr)))
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
        (should-not (plist-get analysis :symbols-with-pos-got))
        (should-not (plist-get analysis :helper))))))

(ert-deftest nelisp-eln-s611-tampered-body-byte-rejected ()
  "Every fixed byte of every body, flipped one at a time, is rejected."
  (skip-unless (file-readable-p nelisp-eln-s611-test--eln))
  (dolist (body nelisp-eln-s611-test--bodies)
    (pcase-let ((`(,shape ,vaddr ,size ,_ ,_) body))
      (let* ((bytes (nelisp-eln-s611-test--slice vaddr size))
             (entry (cdr (assq shape nelisp-eln-tail-code--multi-import-shapes)))
             (template (plist-get entry :template))
             (holes (* 4 (length (plist-get entry :gots))))
             (checked 0))
        (should (= (length template) size))
        (dotimes (i size)
          (when (aref template i)
            (let ((tampered (copy-sequence bytes)))
              (aset tampered i (logxor (aref tampered i) 1))
              (should-not (nelisp-eln-s611-test--analyze tampered vaddr))
              (setq checked (1+ checked)))))
        (should (= checked (- size holes)))))))

(ert-deftest nelisp-eln-s611-port-specs-authenticate ()
  (dolist (shape '(lambda-intern-format lambda-aref-form lambda-cons-form
                   make-closure-form))
    (let ((spec (cdr (assq shape nelisp-eln-native-subr--multi-import-specs))))
      (should spec)
      ;; The spec's ports are the shape's imports, in the same order.
      (should (equal (mapcar #'car (plist-get spec :ports))
                     (plist-get (cdr (assq shape
                                           nelisp-eln-tail-code--multi-import-shapes))
                                :imports)))
      ;; The spec's constants are the shape's d_reloc reads, in order.
      (should (equal (mapcar #'car (plist-get spec :constants))
                     (plist-get (cdr (assq shape
                                           nelisp-eln-tail-code--multi-import-shapes))
                                :data)))
      ;; The host Emacs has no `(builtin funcall)' cell, so only the fixed
      ;; runtime-service ports and the MANY service rows are resolved here
      ;; (the canonical-builtin MANY rows run on NeLisp).
      (dolist (port (plist-get spec :ports))
        (when (or (eq (nth 1 port) 'fixed) (memq (nth 0 port) '(1113 704 1234)))
          (should (plist-get (nelisp-eln-native-subr--multi-port-spec
                              "ba35c031" port)
                             :implementation)))))
    (should (= (nelisp-eln-native-subr-multi-arity (list :shape shape)) 1)))
  ;; The new MANY services are authenticated rows: exactly two arguments.
  (dolist (port '((704 many 3 (lisp lisp lisp) lisp)
                  (704 fixed 2 (lisp lisp) lisp)
                  (1234 many 3 (lisp lisp lisp) lisp)
                  (1234 fixed 2 (lisp lisp) lisp)))
    (should-error (nelisp-eln-native-subr--multi-port-spec "ba35c031" port)
                  :type 'nelisp-eln-native-subr-error))
  ;; The six-argument `Fapply' and the two-argument `Fgtr' are canonical
  ;; builtin rows.
  (should (nelisp-eln-native-subr--many-descriptor "ba35c031" 946 6))
  (should (nelisp-eln-native-subr--many-descriptor "ba35c031" 1318 2))
  (should-not (nelisp-eln-native-subr--many-descriptor "ba35c031" 946 5))
  (should-not (nelisp-eln-native-subr--many-descriptor "ba35c031" 1318 3))
  ;; The new services are what they say.
  (should (equal (nelisp-eln-runtime-services-fformat "V%d" 3) "V3"))
  (should (equal (nelisp-eln-runtime-services-fvconcat '(a) [b]) [a b]))
  (should (eq (nelisp-eln-runtime-services-fintern "s611-probe" nil)
              (intern "s611-probe")))
  (should (equal (nelisp-eln-runtime-services-fmapcar #'1+ '(1 2)) '(2 3)))
  (should (equal (nelisp-eln-runtime-services-fnthcdr 2 '(a b c d)) '(c d)))
  (should (nelisp-eln-runtime-services-fbyte-code-function-p
           (make-byte-code 0 "" [nil] 1)))
  (should-not (nelisp-eln-runtime-services-fbyte-code-function-p '(a)))
  (should (eq (nelisp-eln-runtime-services-faref [x y] 1) 'y)))

(ert-deftest nelisp-eln-s611-runtime-service-descriptors-validate ()
  "The new descriptors agree with the authenticated freloc table."
  (skip-unless (file-readable-p nelisp-eln-runtime-services-freloc-tsv-file))
  (should-not (nelisp-eln-runtime-services-validate-descriptors)))

(ert-deftest nelisp-eln-s611-bytecode-constant-identity ()
  "The closure prototype carries a fifth documentation slot."
  (let ((spec '(:bytecode 257 (192 1 72 135) [V0] 3 "\n\n(fn I)")))
    (should (nelisp-eln-native-subr--multi-constant-matches-p
             (make-byte-code 257 (unibyte-string 192 1 72 135) [V0] 3
                             "\n\n(fn I)")
             spec))
    ;; A changed byte, constant, depth, arglist or documentation string, a
    ;; missing documentation slot, and non-functions.
    (dolist (actual (list (make-byte-code 257 (unibyte-string 192 1 72 136)
                                          [V0] 3 "\n\n(fn I)")
                          (make-byte-code 257 (unibyte-string 192 1 72 135)
                                          [V1] 3 "\n\n(fn I)")
                          (make-byte-code 257 (unibyte-string 192 1 72 135)
                                          [V0] 4 "\n\n(fn I)")
                          (make-byte-code 513 (unibyte-string 192 1 72 135)
                                          [V0] 3 "\n\n(fn I)")
                          (make-byte-code 257 (unibyte-string 192 1 72 135)
                                          [V0] 3 "\n\n(fn J)")
                          (make-byte-code 257 (unibyte-string 192 1 72 135)
                                          [V0] 3)
                          nil 'byte-compile-form))
      (should-not (nelisp-eln-native-subr--multi-constant-matches-p
                   actual spec)))
    ;; A four-component spec still refuses an object with a fifth slot.
    (should-not (nelisp-eln-native-subr--multi-constant-matches-p
                 (make-byte-code 257 (unibyte-string 192 1 72 135) [V0] 3
                                 "\n\n(fn I)")
                 '(:bytecode 257 (192 1 72 135) [V0] 3)))))

(ert-deftest nelisp-eln-s611-registered-lambda-constant ()
  "A `:registered-lambda' constant matches only the artifact's placeholder."
  (should (nelisp-eln-native-subr--multi-constant-matches-p
           "#$" :registered-lambda))
  (dolist (actual (list "#" "#$$" nil 24 'lambda "V%d"))
    (should-not (nelisp-eln-native-subr--multi-constant-matches-p
                 actual :registered-lambda))))

;;; Registration code (`gnu-lambdas-require-subr')

(defun nelisp-eln-s611-test--top ()
  (nelisp-eln-s611-test--slice nelisp-eln-s611-test--top-vaddr 234))

(ert-deftest nelisp-eln-s611-registration-code-decodes ()
  (skip-unless (file-readable-p nelisp-eln-s611-test--eln))
  (let ((top (nelisp-eln-s611-test--top)))
    (should (nelisp-eln-registration--match-holed-template
             top nelisp-eln-registration--gnu-lambdas-require-subr-template
             nelisp-eln-registration--gnu-lambdas-require-subr-holes))
    ;; Every lambda arity 1, the registered subr's arity 1.
    (should (equal (mapcar (lambda (o) (nelisp-eln-registration--gnu-arity top o))
                           '(3 8 84 97 126 139 191 208))
                   '(1 1 1 1 1 1 1 1)))
    ;; Lambda d_reloc indices 24, 34 and 21.
    (should (equal (mapcar (lambda (o)
                             (nelisp-eln-registration--gnu-fixnum-immediate
                              top o))
                           '(64 106 148))
                   '(24 34 21)))
    ;; Lambda type slots 36, 36, 37; subr type slot 36; Feval lexenv 30,
    ;; form 35.
    (should (equal (mapcar (lambda (o)
                             (nelisp-eln-registration--d-reloc-index32 top o))
                           '(52 92 134 199 163 171))
                   '(36 36 37 36 30 35)))
    ;; The earlier profiles' templates are other lengths.
    (dolist (template (list nelisp-eln-registration--gnu-require-subr-template
                            nelisp-eln-registration--gnu-lambda-require-subr-template
                            nelisp-eln-registration--gnu-eval-subr-pair-template
                            nelisp-eln-registration--gnu-eval-subr-template))
      (should-not (= (length template) (length top))))))

(ert-deftest nelisp-eln-s611-tampered-registration-code-rejected ()
  "Every fixed byte of top_level_run, including each import slot
displacement, flipped one at a time, leaves the template unmatched."
  (skip-unless (file-readable-p nelisp-eln-s611-test--eln))
  (let* ((top (nelisp-eln-s611-test--top))
         (holes nelisp-eln-registration--gnu-lambdas-require-subr-holes)
         (checked 0))
    (dotimes (i (length top))
      (unless (nelisp-eln-registration--offset-holed-p i holes)
        (let ((tampered (copy-sequence top)))
          (aset tampered i (logxor (aref tampered i) 1))
          (should-not (nelisp-eln-registration--match-holed-template
                       tampered
                       nelisp-eln-registration--gnu-lambdas-require-subr-template
                       holes))
          (setq checked (1+ checked)))))
    (should (= checked (- 234 (* 4 (length holes)))))
    ;; In particular the five call slots: 1031 three times, 947, 1030.
    (should (equal (mapcar (lambda (o) (+ (aref top o) (* 256 (aref top (1+ o)))))
                           '(71 113 155 178 219))
                   '(#x2038 #x2038 #x2038 #x1d98 #x2030)))))

(defun nelisp-eln-s611-test--metadata ()
  (let ((data (make-vector 41 nil)))
    (aset data 21 "#$") (aset data 24 "#$") (aset data 34 "#$")
    (aset data 30 t)
    (aset data 35 '(require 'bytecomp))
    (aset data 36 '(function (t) t))
    (aset data 37 '(function (t) cons))
    (list :abi-hash "ba35c031"
          :data-relocations data
          :ephemeral-data-relocations
          (vector (concat nelisp-eln-registration--lambda-c-name-prefix "0")
                  '(0 nil nil)
                  (concat nelisp-eln-registration--lambda-c-name-prefix "1")
                  '(1 nil nil)
                  (concat nelisp-eln-registration--lambda-c-name-prefix "2")
                  '(2 nil nil) 'byte-compile-make-closure
                  (nelisp-eln-emitter--symbol-name 'byte-compile-make-closure)
                  '(3 nil nil))
          :function-docs [nil nil nil "\n\n(fn FORM)"]
          :d-reloc-size 328 :d-reloc-eph-size 72)))

(defconst nelisp-eln-s611-test--extra
  '(:type-index 36 :lexenv-index 30 :form-index 35
    :lambdas-spec ((24 1 36) (34 1 36) (21 1 37))))

(defun nelisp-eln-s611-test--bad-count (metadata extra)
  (cadr (should-error
         (nelisp-eln-registration--metadata-data-count
          metadata 'gnu-lambdas-require-subr 1 extra)
         :type 'nelisp-eln-registration-error)))

(ert-deftest nelisp-eln-s611-metadata-envelope ()
  (let ((metadata (nelisp-eln-s611-test--metadata)))
    (should (= (nelisp-eln-registration--metadata-data-count
                metadata 'gnu-lambdas-require-subr 1
                nelisp-eln-s611-test--extra)
               41))
    (should (eq (nelisp-eln-registration--require-effect metadata 35 30)
                'bytecomp))
    ;; A lambda registered into a slot that is not a "#$" placeholder (a
    ;; symbol slot, out of range, the Feval form/lexenv slots, the type
    ;; slot), with the wrong arity for its type, under a type slot that is
    ;; not a fixed-arity function type, or with no type at all.
    (dolist (spec-edit '((0 . (25 1 36)) (0 . (99 1 36)) (1 . (35 1 36))
                         (1 . (30 1 36)) (2 . (36 1 37))
                         (0 . (24 2 36)) (0 . (24 1 21)) (2 . (21 1 30))
                         (2 . (21 1 0)) (0 . (24 1 nil))))
      (let ((specs (copy-sequence
                    (plist-get nelisp-eln-s611-test--extra :lambdas-spec))))
        (setf (nth (car spec-edit) specs) (cdr spec-edit))
        (should (eq (nelisp-eln-s611-test--bad-count
                     metadata (plist-put (copy-sequence
                                          nelisp-eln-s611-test--extra)
                                         :lambdas-spec specs))
                    'metadata-outside-emitter-slice))))
    ;; Two or four lambdas, a wrong subr type slot, a docs vector too short
    ;; for the fourth descriptor.
    (dolist (extra (list (plist-put (copy-sequence nelisp-eln-s611-test--extra)
                                    :lambdas-spec '((24 1 36) (34 1 36)))
                         (plist-put (copy-sequence nelisp-eln-s611-test--extra)
                                    :lambdas-spec
                                    '((24 1 36) (34 1 36) (21 1 37) (21 1 37)))
                         (plist-put (copy-sequence nelisp-eln-s611-test--extra)
                                    :type-index 30)))
      (should (eq (nelisp-eln-s611-test--bad-count metadata extra)
                  'metadata-outside-emitter-slice)))
    (should (eq (nelisp-eln-s611-test--bad-count
                 (plist-put (copy-sequence metadata) :function-docs
                            [nil nil "\n\n(fn FORM)"])
                 nelisp-eln-s611-test--extra)
                'metadata-outside-emitter-slice))
    ;; Wrong lambda C names or descriptors are refused.
    (dolist (edit '((0 . "F616e6f6e796d6f75732d6c616d626461_anonymous_lambda_x")
                    (0 . "F616e6f6e796d6f75732d6c616d626461_anonymous_lambda_1")
                    (0 . "some_other_function")
                    (1 . (0 t nil))
                    (3 . (2 nil nil))
                    (5 . (1 nil nil))
                    (8 . (0 nil nil))))
      (let ((bad (copy-sequence (nelisp-eln-s611-test--metadata))))
        (let ((eph (copy-sequence (plist-get bad :ephemeral-data-relocations))))
          (aset eph (car edit) (cdr edit))
          (setq bad (plist-put bad :ephemeral-data-relocations eph)))
        (should (eq (nelisp-eln-s611-test--bad-count
                     bad nelisp-eln-s611-test--extra)
                    'metadata-outside-emitter-slice))))))

;;; Shared port numbering

(defun nelisp-eln-s611-test--fake-analysis (&rest ports)
  "An analysis over PORTS, each (SLOT CONVENTION ARITY KINDS RETURN)."
  (list :imports (mapcar (lambda (p) (list :slot (nth 0 p))) ports)
        :port-specs (mapcar (lambda (p)
                              (list :slot (nth 0 p) :convention (nth 1 p)
                                    :arity (nth 2 p) :arguments (nth 3 p)
                                    :return (nth 4 p)
                                    :implementation 'ignore))
                            ports)))

(ert-deftest nelisp-eln-s611-shared-port-numbering ()
  (let* ((main (nelisp-eln-s611-test--fake-analysis
                '(9 fixed 2 (lisp lisp) lisp) '(5 fixed 1 (lisp) lisp)
                '(7 many 2 (lisp lisp) lisp)))
         (a (nelisp-eln-s611-test--fake-analysis
             '(7 many 2 (lisp lisp) lisp) '(11 fixed 2 (lisp lisp) lisp)))
         (b (nelisp-eln-s611-test--fake-analysis
             '(5 fixed 1 (lisp) lisp) '(9 fixed 2 (lisp lisp) lisp)))
         (numbered (nelisp-eln-registration--assign-port-numbers
                    (list main a b))))
    ;; Main first, in import order; later bodies' new slots follow.
    (should (equal (mapcar (lambda (x) (plist-get x :port-numbers)) numbered)
                   '((0 1 2) (2 3) (1 0))))
    ;; Every entry keeps its own proof.
    (should (equal (plist-get (nth 1 numbered) :imports)
                   (plist-get a :imports))))
  ;; A slot two bodies import must carry the same authenticated descriptor.
  (dolist (bad '(((7 many 2 (lisp lisp) handle-nil))
                 ((7 many 3 (lisp lisp lisp) lisp))
                 ((7 fixed 2 (lisp lisp) lisp))
                 ((7 many 2 (lisp raw) lisp))))
    (should (eq (cadr (should-error
                       (nelisp-eln-registration--assign-port-numbers
                        (list (nelisp-eln-s611-test--fake-analysis
                               '(7 many 2 (lisp lisp) lisp))
                              (apply #'nelisp-eln-s611-test--fake-analysis
                                     bad)))
                       :type 'nelisp-eln-registration-error))
                'shared-slot-descriptor-mismatch)))
  ;; More distinct slots than the runtime has ports.
  (should (eq (cadr (should-error
                     (nelisp-eln-registration--assign-port-numbers
                      (list (apply #'nelisp-eln-s611-test--fake-analysis
                                   (mapcar (lambda (slot)
                                             (list slot 'fixed 1 '(lisp) 'lisp))
                                           (number-sequence 100 132)))))
                     :type 'nelisp-eln-registration-error))
              'too-many-shared-ports))
  ;; An analysis without ports is malformed.
  (should (eq (cadr (should-error
                     (nelisp-eln-registration--assign-port-numbers
                      (list (list :imports nil :port-specs nil)))
                     :type 'nelisp-eln-registration-error))
              'port-numbering-malformed-proof))
  ;; Without `:port-numbers' the numbering stays sequential.
  (should (= (nelisp-eln-native-subr--port-number '(:imports nil) 3) 3))
  (should (= (nelisp-eln-native-subr--port-number '(:port-numbers (4 2)) 1) 2))
  (should-error (nelisp-eln-native-subr--port-number '(:port-numbers (4)) 1)
                :type 'nelisp-eln-native-subr-error))

;;; Registered-lambda constants resolve per call, and only while live

(ert-deftest nelisp-eln-s611-registered-lambda-resolution ()
  (let* ((callable (lambda (_x) 'lambda-24))
         (owner (make-vector nelisp-eln-registration--owner-size nil))
         (lease (vector nil nil owner nil nil nil nil nil))
         (words '((#x1000 . #x7005) (#x1008 . #x9000) (#x1010 . 42))))
    (aset owner 18 (list :lambda-callables (list (cons 24 callable))
                         :lambda-slot-words '((24 . #x7005))))
    (cl-letf (((symbol-function 'nelisp-eln-abi-read-word)
               (lambda (address _offset) (cdr (assq address words)))))
      ;; A live slot resolves to the owner's callable; an ordinary constant
      ;; passes through with its live word.
      (should (equal (nelisp-eln-native-subr--resolve-constant-cells
                      '((#x1000 . (:registered-lambda . 24))
                        (#x1010 . quote))
                      lease)
                     (list (cons #x7005 callable) (cons 42 'quote))))
      ;; The slot's word must be exactly the one the owner published.
      (dolist (cell '((#x1008 . (:registered-lambda . 24))
                      (#x1000 . (:registered-lambda . 34))
                      (#x1000 . (:registered-lambda . 21))))
        (should (eq (cadr (should-error
                           (nelisp-eln-native-subr--resolve-constant-cells
                            (list cell) lease)
                           :type 'nelisp-eln-native-subr-error))
                    'registered-lambda-slot-not-live)))
      ;; A lease of no owner resolves nothing.
      (should-error (nelisp-eln-native-subr--resolve-constant-cells
                     '((#x1000 . (:registered-lambda . 24)))
                     (vector nil nil nil nil nil nil nil nil))
                    :type 'nelisp-eln-native-subr-error))))

(ert-deftest nelisp-eln-s611-lambda-lease-lookup ()
  "A lambda's lease is valid only for the capability the owner kept with it."
  (let* ((owner (make-vector nelisp-eln-registration--owner-size nil))
         (lease-a (vector nelisp-eln-native-subr--tail-lease-marker nil owner
                          nil nil nil nil nil))
         (lease-b (vector nelisp-eln-native-subr--tail-lease-marker nil owner
                          nil nil nil nil nil))
         (cap-a '(x nil "a" 4096 "h" 1 71 root))
         (cap-b '(x nil "b" 8192 "h" 1 27 root)))
    (aset owner 18 (list :lambda-leases (list (cons lease-a cap-a))))
    ;; The lease `lease-b' the owner never recorded, and a recorded lease
    ;; presented with another lambda's capability, both fail closed (the
    ;; handle/table checks that follow are not reached).
    (should-not (nelisp-eln-native-subr--multi-lease-valid-p
                 lease-b nil cap-b))
    (should-not (nelisp-eln-native-subr--multi-lease-valid-p
                 lease-a nil cap-b))))

;;; Role sequence: nothing beyond the admitted calls

(defun nelisp-eln-s611-test--owner ()
  (let ((owner (make-vector nelisp-eln-registration--owner-size nil)))
    (aset owner 18 (list :role-sequence
                         '(lambda lambda lambda require register)
                         :lambda-callable t
                         :lambdas (list (list :idx 24 :c-name "a" :arity 1)
                                        (list :idx 34 :c-name "b" :arity 1)
                                        (list :idx 21 :c-name "c" :arity 1))))
    owner))

(ert-deftest nelisp-eln-s611-role-sequence ()
  (let ((owner (nelisp-eln-s611-test--owner)))
    (let ((roles nil))
      (dolist (n '(1 2 3 4 5 6 7))
        (let ((nelisp-eln-registration--call-index n))
          (push (nelisp-eln-registration--call-role owner) roles)))
      (should (equal (nreverse roles)
                     '(lambda lambda lambda require register nil nil))))
    (let ((ordinals nil))
      (dolist (n '(1 2 3))
        (let ((nelisp-eln-registration--call-index n))
          (push (nelisp-eln-registration--lambda-ordinal owner) ordinals)))
      (should (equal (nreverse ordinals) '(1 2 3))))
    (let ((nelisp-eln-registration--call-index 5))
      (should (= (nelisp-eln-registration--register-ordinal owner) 1)))))

(ert-deftest nelisp-eln-s611-extra-registration-rejected ()
  "A call past the fifth admitted one (an extra registration) is refused."
  (let* ((owner (nelisp-eln-s611-test--owner))
         (nelisp-eln-registration--owners (list owner))
         (nelisp-eln-registration--active-owner owner)
         (nelisp-eln-registration--call-index 5))
    (should (eq (car (should-error (nelisp-eln-registration--callback 0)
                                   :type 'nelisp-eln-registration-error))
                'nelisp-eln-registration-error))
    (should (eq (cadr nelisp-eln-registration--last-callback-error)
                'callback-sequence-not-admitted))
    ;; The failure is sticky: a later callback that succeeds cannot hide it.
    (should (eq (cadr (car nelisp-eln-registration--callback-failures))
                'callback-sequence-not-admitted)))
  (setq nelisp-eln-registration--callback-failures nil))

(ert-deftest nelisp-eln-s611-unadmitted-lambda-callback-rejected ()
  "A callable `register_lambda' callback with no owner, an ordinal outside
1..3, or an owner that does not expect it is refused before anything is
built."
  (let ((nelisp-eln-registration--active-owner nil))
    (cl-letf (((symbol-function 'nelisp-eln-abi-read-word)
               (lambda (_address _offset) 0)))
      (should (eq (cadr (should-error
                         (nelisp-eln-registration--register-lambda-callable-callback
                          #x10000 1)
                         :type 'nelisp-eln-registration-error))
                  'callback-arguments-not-admitted))))
  (let* ((owner (nelisp-eln-s611-test--owner))
         (nelisp-eln-registration--active-owner owner))
    (cl-letf (((symbol-function 'nelisp-eln-abi-read-word)
               (lambda (_address _offset) 0)))
      (dolist (ordinal '(0 4 5))
        (should (eq (cadr (should-error
                           (nelisp-eln-registration--register-lambda-callable-callback
                            #x10000 ordinal)
                           :type 'nelisp-eln-registration-error))
                    'callback-arguments-not-admitted))))))

;;; End-to-end on a standalone binary (optional)

(defconst nelisp-eln-s611-test--root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name
                             default-directory))))
  "Repository root, captured while this file loads.")

(defun nelisp-eln-s611-test--bin ()
  (let ((bin (getenv "NELISP_S611_BIN")))
    (and bin (file-executable-p bin) bin)))

(defvar nelisp-eln-s611-test--wrapper nil)

(defun nelisp-eln-s611-test--normal-load-wrapper (dir)
  "Return the normal-load wrapper generated the way the S6 harness does."
  (or (and nelisp-eln-s611-test--wrapper
           (file-readable-p nelisp-eln-s611-test--wrapper)
           nelisp-eln-s611-test--wrapper)
      (let ((wrapper (expand-file-name "normal-load-wrapper.el" dir))
            (default-directory nelisp-eln-s611-test--root))
        (with-temp-buffer
          (should (eql 0 (call-process
                          (or (getenv "EMACS") "emacs") nil t nil
                          "--batch" "-Q" "-L" "scripts" "-L" "lisp" "--eval"
                          "(progn (defvar nelisp-standalone--repo-root (file-name-as-directory default-directory)) (dolist (name (list \"nelisp-standalone--core-bytecode-src\" \"nelisp-standalone--after-load-runtime-src\")) (with-temp-buffer (insert-file-contents \"scripts/nelisp-standalone-build.el\") (goto-char (point-min)) (unless (search-forward (concat \"(defun \" name) nil t) (error \"source generator not found: %s\" name)) (goto-char (match-beginning 0)) (eval (read (current-buffer))))) (princ (nelisp-standalone--after-load-runtime-src)))")))
          (write-region (point-min) (point-max) wrapper nil 'silent))
        (setq nelisp-eln-s611-test--wrapper wrapper))))

(defun nelisp-eln-s611-test--run-probe (eln probe-body)
  "Run PROBE-BODY, a Lisp form string, on the standalone binary with ELN
named by the variable `eln'; return the standard output."
  (let* ((dir (make-temp-file "s611-probe" t))
         (probe (expand-file-name "probe.el" dir))
         (bin (nelisp-eln-s611-test--bin))
         (cold (concat bin ".cold"))
         (default-directory nelisp-eln-s611-test--root))
    (unwind-protect
        (progn
          (let ((wrapper (nelisp-eln-s611-test--normal-load-wrapper dir)))
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

(defconst nelisp-eln-s611-test--load-probe
  "(condition-case e (progn (nelisp-eln-registration-load eln) (princ \"LOADED\\n\")) (error (princ (format \"REJECT %S %S\\n\" (car e) (cadr e)))))")

(defun nelisp-eln-s611-test--tampered-copy (dir offset expected new)
  (let ((eln (expand-file-name "gnu-byte-compile-make-closure.eln" dir)))
    (copy-file nelisp-eln-s611-test--eln eln)
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

(defun nelisp-eln-s611-test--run-tampered (offset expected new reason)
  "Load a copy with file byte OFFSET (EXPECTED) set to NEW on the standalone
binary and require the registration to be rejected naming REASON."
  (let ((dir (make-temp-file "s611-tamper" t)))
    (unwind-protect
        (let ((out (nelisp-eln-s611-test--run-probe
                    (nelisp-eln-s611-test--tampered-copy
                     dir offset expected new)
                    nelisp-eln-s611-test--load-probe)))
          (should (string-match-p "^REJECT " out))
          (should (string-match-p reason out)))
      (delete-directory dir t))))

(defmacro nelisp-eln-s611-test--e2e (name doc offset expected new reason)
  `(ert-deftest ,name ()
     ,doc
     (skip-unless (and (nelisp-eln-s611-test--bin)
                       (file-readable-p nelisp-eln-s611-test--eln)))
     (nelisp-eln-s611-test--run-tampered ,offset ,expected ,new ,reason)))

(nelisp-eln-s611-test--e2e
 nelisp-eln-s611-e2e-tampered-lambda-0-body-rejected
 "The first lambda's `mov $2,%edi' immediate (the `Fformat' argc)."
 #x1107 #x02 #x06 "leaf-instructions-not-admitted")

(nelisp-eln-s611-test--e2e
 nelisp-eln-s611-e2e-tampered-lambda-1-body-rejected
 "The second lambda's `mov $2,%edi' immediate."
 #x115b #x02 #x06 "leaf-instructions-not-admitted")

(nelisp-eln-s611-test--e2e
 nelisp-eln-s611-e2e-tampered-lambda-2-body-rejected
 "The third lambda's `push %rbx' -> `push %rdx'."
 #x1177 #x53 #x52 "leaf-instructions-not-admitted")

(nelisp-eln-s611-test--e2e
 nelisp-eln-s611-e2e-tampered-lambda-call-target-rejected
 "The third lambda's first `call *0x22f8(%rbx)' (slot 1119) redirected to
slot 1120: an unauthenticated call target inside a lambda body."
 #x117f #xf8 #x00 "leaf-instructions-not-admitted")

(nelisp-eln-s611-test--e2e
 nelisp-eln-s611-e2e-lambda-got-displacement-rejected
 "The second lambda's freloc GOT load displacement moved by eight bytes."
 #x1153 #x81 #x89 "REJECT")

(nelisp-eln-s611-test--e2e
 nelisp-eln-s611-e2e-tampered-main-body-rejected
 "The subr body's first `push %r15' -> `push %r14'."
 #x11a1 #x57 #x56 "leaf-instructions-not-admitted")

(nelisp-eln-s611-test--e2e
 nelisp-eln-s611-e2e-tampered-lambda-padding-rejected
 "The alignment padding after the first lambda."
 #x1147 #x66 #x67 "executable-region-not-admitted")

(nelisp-eln-s611-test--e2e
 nelisp-eln-s611-e2e-lambda-via-unauthenticated-slot-rejected
 "The second `register_lambda' call goes through slot 1032, not 1031."
 #x18a1 #x38 #x40 "top-level-instructions-not-admitted")

(nelisp-eln-s611-test--e2e
 nelisp-eln-s611-e2e-lambdas-into-one-slot-rejected
 "The second lambda registered into d_reloc[24], the first one's slot."
 #x189a #x8a #x62 "top-level-instructions-not-admitted")

(nelisp-eln-s611-test--e2e
 nelisp-eln-s611-e2e-lambda-into-form-slot-rejected
 "The second lambda registered over d_reloc[35] (the require form)."
 #x189a #x8a #x8e "top-level-instructions-not-admitted")

(nelisp-eln-s611-test--e2e
 nelisp-eln-s611-e2e-lambda-into-non-placeholder-slot-rejected
 "The second lambda registered over d_reloc[25], which holds a symbol."
 #x189a #x8a #x66 "metadata-outside-emitter-slice")

(nelisp-eln-s611-test--e2e
 nelisp-eln-s611-e2e-lambda-arity-mismatch-rejected
 "The second lambda's maximum arity raised to 2 while its minimum stays 1."
 #x1884 #x06 #x0a "top-level-instructions-not-admitted")

(ert-deftest nelisp-eln-s611-e2e-registered-lambdas-are-callable-subrs ()
  "The three registered lambdas are real native subrs: right results, a
wrong argument count refused, and none of them published under a name."
  (skip-unless (and (nelisp-eln-s611-test--bin)
                    (file-readable-p nelisp-eln-s611-test--eln)))
  (let ((out (nelisp-eln-s611-test--run-probe
              nelisp-eln-s611-test--eln
              "(condition-case e
  (progn
    (nelisp-eln-registration-load eln)
    (let* ((owner (car nelisp-eln-registration--owners))
           (callables (plist-get (aref owner 18) :lambda-callables)))
      (princ (format \"COUNT %S\\n\" (length callables)))
      (princ (format \"IDX %S\\n\" (mapcar #'car callables)))
      (princ (format \"ARITY %S\\n\" (mapcar (lambda (c) (func-arity (cdr c))) callables)))
      (princ (format \"CALL0 %S\\n\" (funcall (cdr (nth 0 callables)) 3)))
      (princ (format \"CALL2 %S\\n\" (funcall (cdr (nth 2 callables)) 'x)))
      (dolist (c callables)
        (dolist (args '(() (1 2)))
          (princ (format \"WRONG-ARITY %S %S\\n\" (car c)
                         (condition-case e2 (apply (cdr c) args)
                           (wrong-number-of-arguments 'wrong-number-of-arguments)
                           (error (car e2)))))))
      (princ (format \"UNBOUND %S\\n\"
                     (mapcar (lambda (i) (fboundp (intern (format \"F616e6f6e796d6f75732d6c616d626461_anonymous_lambda_%d\" i)))) '(0 1 2))))))
  (error (princ (format \"PROBE-ERROR %S\\n\" (car e)))))")))
    (should (string-match-p "^COUNT 3$" out))
    (should (string-match-p "^IDX (24 34 21)$" out))
    (should (string-match-p "^ARITY ((1 \\. 1) (1 \\. 1) (1 \\. 1))$" out))
    (should (string-match-p "^CALL0 V3$" out))
    (should (string-match-p "^CALL2 'x$" out))
    (dolist (idx '(24 34 21))
      (should (string-match-p
               (format "^WRONG-ARITY %d wrong-number-of-arguments$" idx) out)))
    (should-not (string-match-p "PROBE-ERROR" out))
    (should (string-match-p "^UNBOUND (nil nil nil)$" out))))

(provide 'nelisp-eln-s611-closure-admission-test)

;;; nelisp-eln-s611-closure-admission-test.el ends here
