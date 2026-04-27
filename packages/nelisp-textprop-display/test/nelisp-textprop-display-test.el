;;; nelisp-textprop-display-test.el --- ERT for display property + validator  -*- lexical-binding: t; -*-

;; T144 / Phase 9c.2 — exercises `nelisp-textprop-display' per Doc 41
;; §3.2.4 ERT gate.  Coverage targets the 8 v0 display types (Doc 41
;; §2.4 LOCKED) with valid + invalid pairs, plus the validator /
;; accessor / merge / resolve quartet (= the §3.2.1 + §3.2.2 ship gate).
;;
;; Layout:
;;   §A. predicates / known types
;;   §B. valid spec acceptance (8 types + cascade + sugar)
;;   §C. invalid spec rejection (8 types + bad form)
;;   §D. nested specs (when / replace)
;;   §E. accessor round-trip (type / payload / replace-spec / attribute)
;;   §F. attribute query + merge
;;   §G. resolve canonicalisation
;;   §H. nesting-limit guard
;;   §I. backend-absent storage / query (Doc 41 §3.2.4 last bullet)

(require 'ert)
(require 'cl-lib)
(require 'nelisp-textprop-display)

;;; ─────────────────────────────────────────────────────────────────────
;;; §A. predicates / known types
;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-display-known-types-set ()
  (let ((types (nelisp-display-known-types)))
    (should (memq 'string types))
    (should (memq 'image types))
    (should (memq 'space types))
    (should (memq 'height types))
    (should (memq 'raise types))
    (should (memq 'margin types))
    (should (memq 'slice types))
    (should (memq 'when types))
    (should (memq 'replace types))
    ;; Returned list is a fresh copy.
    (should-not (eq types (nelisp-display-known-types)))))

(ert-deftest nelisp-display-type-p-detects-known ()
  (should (nelisp-display-type-p 'image))
  (should (nelisp-display-type-p 'height))
  (should (nelisp-display-type-p 'when))
  (should-not (nelisp-display-type-p 'unknown-type))
  (should-not (nelisp-display-type-p nil))
  (should-not (nelisp-display-type-p "string")))

(ert-deftest nelisp-display-spec-p-suppresses-signal ()
  (should (nelisp-display-spec-p "literal"))
  (should (nelisp-display-spec-p '(height 1.5)))
  (should-not (nelisp-display-spec-p 42))
  (should-not (nelisp-display-spec-p '(unknown-type 1))))

(ert-deftest nelisp-display-contract-version-locked ()
  (should (= nelisp-display-property-contract-version 1)))

;;; ─────────────────────────────────────────────────────────────────────
;;; §B. valid spec acceptance — 8 types + sugar + cascade
;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-display-valid-string ()
  (should (eq (nelisp-display-spec-validate "literal text") t))
  (should (eq (nelisp-display-spec-validate "") t))
  (should (eq (nelisp-display-spec-validate nil) t)))

(ert-deftest nelisp-display-valid-image ()
  (should (eq (nelisp-display-spec-validate '(image :type png :file "x")) t))
  (should (eq (nelisp-display-spec-validate '(image :type svg)) t)))

(ert-deftest nelisp-display-valid-space ()
  (should (eq (nelisp-display-spec-validate '(space :width 5)) t))
  (should (eq (nelisp-display-spec-validate '(space :width 1.0 :height 2)) t)))

(ert-deftest nelisp-display-valid-height ()
  (should (eq (nelisp-display-spec-validate '(height 1.2)) t))
  (should (eq (nelisp-display-spec-validate '(height 2)) t))
  (should (eq (nelisp-display-spec-validate '(height (3 . 2))) t))
  (should (eq (nelisp-display-spec-validate '(height bigger)) t))
  ;; Function-call form (per Emacs precedent: (+ 2), (* 1.5))
  (should (eq (nelisp-display-spec-validate '(height (+ 2))) t)))

(ert-deftest nelisp-display-valid-raise ()
  (should (eq (nelisp-display-spec-validate '(raise 0.5)) t))
  (should (eq (nelisp-display-spec-validate '(raise -0.3)) t)))

(ert-deftest nelisp-display-valid-margin ()
  (should (eq (nelisp-display-spec-validate '(margin left-margin)) t))
  (should (eq (nelisp-display-spec-validate '(margin right-margin)) t)))

(ert-deftest nelisp-display-valid-slice ()
  (should (eq (nelisp-display-spec-validate '(slice 0 0 10 10)) t))
  (should (eq (nelisp-display-spec-validate '(slice 1.5 2.5 3.5 4.5)) t)))

(ert-deftest nelisp-display-valid-when ()
  (should (eq (nelisp-display-spec-validate '(when t "shown")) t))
  (should (eq (nelisp-display-spec-validate '(when (eq major-mode 'org-mode)
                                                "shown"))
              t)))

(ert-deftest nelisp-display-valid-replace ()
  (should (eq (nelisp-display-spec-validate '(replace "X")) t))
  (should (eq (nelisp-display-spec-validate '(replace (image :type png))) t)))

(ert-deftest nelisp-display-valid-cascade ()
  (should (eq (nelisp-display-spec-validate
               '("prefix" (height 1.2) (raise 0.1)))
              t)))

;;; ─────────────────────────────────────────────────────────────────────
;;; §C. invalid spec rejection — 8 types + bad shape
;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-display-invalid-image-bad-plist ()
  (should-error (nelisp-display-spec-validate '(image :type))
                :type 'nelisp-display-bad-spec))

(ert-deftest nelisp-display-invalid-space-bad-plist ()
  (should-error (nelisp-display-spec-validate '(space :width))
                :type 'nelisp-display-bad-spec))

(ert-deftest nelisp-display-invalid-height-no-arg ()
  (should-error (nelisp-display-spec-validate '(height))
                :type 'nelisp-display-bad-spec))

(ert-deftest nelisp-display-invalid-height-extra-arg ()
  (should-error (nelisp-display-spec-validate '(height 1.0 2.0))
                :type 'nelisp-display-bad-spec))

(ert-deftest nelisp-display-invalid-raise-non-number ()
  (should-error (nelisp-display-spec-validate '(raise "x"))
                :type 'nelisp-display-bad-spec))

(ert-deftest nelisp-display-invalid-margin-bad-symbol ()
  (should-error (nelisp-display-spec-validate '(margin top-margin))
                :type 'nelisp-display-bad-spec))

(ert-deftest nelisp-display-invalid-slice-wrong-arity ()
  (should-error (nelisp-display-spec-validate '(slice 0 0 10))
                :type 'nelisp-display-bad-spec))

(ert-deftest nelisp-display-invalid-slice-non-number ()
  (should-error (nelisp-display-spec-validate '(slice 0 0 "10" 10))
                :type 'nelisp-display-bad-spec))

(ert-deftest nelisp-display-invalid-when-missing-spec ()
  (should-error (nelisp-display-spec-validate '(when t))
                :type 'nelisp-display-bad-spec))

(ert-deftest nelisp-display-invalid-replace-no-arg ()
  (should-error (nelisp-display-spec-validate '(replace))
                :type 'nelisp-display-bad-spec))

(ert-deftest nelisp-display-invalid-form-non-cons ()
  (should-error (nelisp-display-spec-validate 42)
                :type 'nelisp-display-bad-spec))

(ert-deftest nelisp-display-invalid-unknown-type-symbol ()
  ;; A bare unknown symbol isn't a cascade element either; should reject.
  (should-error (nelisp-display-spec-validate '(unknown-type-x 1 2))
                :type 'nelisp-display-error))

;;; ─────────────────────────────────────────────────────────────────────
;;; §D. nested specs — when / replace
;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-display-nested-when-string ()
  (should (eq (nelisp-display-spec-validate '(when t "X")) t)))

(ert-deftest nelisp-display-nested-replace-image ()
  (should (eq (nelisp-display-spec-validate
               '(replace (image :type png :file "x.png")))
              t)))

(ert-deftest nelisp-display-nested-when-replace-image ()
  (should (eq (nelisp-display-spec-validate
               '(when t (replace (image :type svg))))
              t)))

(ert-deftest nelisp-display-nested-replace-rejects-bad-inner ()
  (should-error (nelisp-display-spec-validate '(replace (height "x")))
                :type 'nelisp-display-bad-spec))

;;; ─────────────────────────────────────────────────────────────────────
;;; §E. accessor round-trip
;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-display-accessor-string-type ()
  (should (eq (nelisp-display-spec-type "X") 'string))
  (should (equal (nelisp-display-spec-payload "X") "X")))

(ert-deftest nelisp-display-accessor-image-type ()
  (let ((spec '(image :type png :file "x.png")))
    (should (eq (nelisp-display-spec-type spec) 'image))
    (should (equal (nelisp-display-spec-payload spec)
                   '(:type png :file "x.png")))))

(ert-deftest nelisp-display-accessor-height-type ()
  (let ((spec '(height 1.5)))
    (should (eq (nelisp-display-spec-type spec) 'height))
    (should (equal (nelisp-display-spec-payload spec) 1.5))))

(ert-deftest nelisp-display-accessor-margin-type ()
  (let ((spec '(margin left-margin)))
    (should (eq (nelisp-display-spec-type spec) 'margin))
    (should (eq (nelisp-display-spec-payload spec) 'left-margin))))

(ert-deftest nelisp-display-accessor-slice-payload ()
  (let ((spec '(slice 1 2 3 4)))
    (should (eq (nelisp-display-spec-type spec) 'slice))
    (should (equal (nelisp-display-spec-payload spec) '(1 2 3 4)))))

(ert-deftest nelisp-display-accessor-when-payload ()
  (let ((spec '(when foo "shown")))
    (should (eq (nelisp-display-spec-type spec) 'when))
    (should (equal (nelisp-display-spec-payload spec) '(foo "shown")))))

(ert-deftest nelisp-display-accessor-replace-spec ()
  (should (equal (nelisp-display-spec-replace-spec '(replace "X")) "X"))
  (should (equal (nelisp-display-spec-replace-spec '(when t (image :type png)))
                 '(image :type png)))
  (should-not (nelisp-display-spec-replace-spec '(height 1.0))))

(ert-deftest nelisp-display-accessor-cascade-type ()
  (should (eq (nelisp-display-spec-type '("X" (height 1.2))) 'cascade)))

(ert-deftest nelisp-display-accessor-nil-type ()
  (should-not (nelisp-display-spec-type nil)))

;;; ─────────────────────────────────────────────────────────────────────
;;; §F. attribute query + merge
;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-display-spec-attribute-image ()
  (should (eq (nelisp-display-spec-attribute '(image :type png) :type) 'png))
  (should (equal (nelisp-display-spec-attribute
                  '(image :type png :file "x") :file)
                 "x")))

(ert-deftest nelisp-display-spec-attribute-non-typed-returns-nil ()
  (should-not (nelisp-display-spec-attribute "X" :type))
  (should-not (nelisp-display-spec-attribute '(height 1.0) :type)))

(ert-deftest nelisp-display-spec-attribute-rejects-non-keyword ()
  (should-error (nelisp-display-spec-attribute '(image :type png) 'type)
                :type 'nelisp-display-bad-attribute))

(ert-deftest nelisp-display-attribute-cascade-first-wins ()
  (let ((spec '((image :type svg) (image :type png))))
    ;; First contributing spec wins.
    (should (eq (nelisp-display-attribute spec :type) 'svg))))

(ert-deftest nelisp-display-merge-disjoint-types ()
  (let ((merged (nelisp-display-merge "prefix" '(height 1.2))))
    (should (= (length merged) 2))
    (should (member "prefix" merged))
    (should (member '(height 1.2) merged))))

(ert-deftest nelisp-display-merge-suppresses-duplicate-type ()
  ;; Both inputs carry a `height' spec — left wins; right suppressed.
  (let ((merged (nelisp-display-merge '(height 1.2) '(height 0.8))))
    (should (member '(height 1.2) merged))
    (should-not (member '(height 0.8) merged))))

(ert-deftest nelisp-display-merge-handles-cascades ()
  (let ((merged (nelisp-display-merge
                 '("prefix" (height 1.2))
                 '((raise 0.1) (height 0.8)))))
    ;; height left wins, raise added from right, prefix kept.
    (should (member "prefix" merged))
    (should (member '(height 1.2) merged))
    (should (member '(raise 0.1) merged))
    (should-not (member '(height 0.8) merged))))

;;; ─────────────────────────────────────────────────────────────────────
;;; §G. resolve canonicalisation
;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-display-resolve-string-becomes-list ()
  (should (equal (nelisp-display-resolve "X" nil) '("X"))))

(ert-deftest nelisp-display-resolve-typed-becomes-singleton ()
  (should (equal (nelisp-display-resolve '(height 1.2) nil)
                 '((height 1.2)))))

(ert-deftest nelisp-display-resolve-cascade-passthrough ()
  (let* ((src '("X" (height 1.2)))
         (out (nelisp-display-resolve src nil)))
    (should (equal out src))
    ;; Output is a *fresh* list — caller may mutate without harm.
    (should-not (eq out src))))

(ert-deftest nelisp-display-resolve-nil-stays-nil ()
  (should-not (nelisp-display-resolve nil nil)))

(ert-deftest nelisp-display-resolve-frame-ignored-v1 ()
  ;; Doc 41 v1 contract: FRAME is informational only; resolve must
  ;; produce identical output regardless of FRAME.
  (let ((a (nelisp-display-resolve '(height 1.2) nil))
        (b (nelisp-display-resolve '(height 1.2) 'some-frame-stub)))
    (should (equal a b))))

;;; ─────────────────────────────────────────────────────────────────────
;;; §H. nesting-limit guard
;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-display-nesting-limit-rejects-deep ()
  ;; Build a deeply nested (when t (when t ... "X")) past the limit.
  (let ((spec "X"))
    (dotimes (_ (1+ nelisp-display-spec-nesting-limit))
      (setq spec (list 'when t spec)))
    (should-error (nelisp-display-spec-validate spec)
                  :type 'nelisp-display-bad-spec)))

(ert-deftest nelisp-display-nesting-limit-allows-shallow ()
  ;; A 3-deep nesting stays well within the limit.
  (should (eq (nelisp-display-spec-validate
               '(when t (when t (replace "X"))))
              t)))

;;; ─────────────────────────────────────────────────────────────────────
;;; §I. backend-absent storage / query (Doc 41 §3.2.4 last bullet)
;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-display-no-backend-validate-all-types ()
  ;; With no Phase 11 backend in the picture, every Doc 41 §2.4 type
  ;; must still validate + accessor-roundtrip purely on this layer.
  (dolist (case '((string . "literal")
                  (image . (image :type png))
                  (space . (space :width 5))
                  (height . (height 1.2))
                  (raise . (raise 0.5))
                  (margin . (margin left-margin))
                  (slice . (slice 0 0 10 10))
                  (when . (when t "X"))
                  (replace . (replace "Y"))))
    (let ((expected-type (car case))
          (spec (cdr case)))
      (should (nelisp-display-spec-validate spec))
      ;; `string' sugar uses string predicate; others match head.
      (should (eq (nelisp-display-spec-type spec) expected-type)))))

(ert-deftest nelisp-display-no-backend-attribute-no-op ()
  ;; Attribute query on non-image / non-space spec yields nil (= no-op),
  ;; so storage-only callers can safely query without a backend.
  (should-not (nelisp-display-attribute '(height 1.2) :width))
  (should-not (nelisp-display-attribute "literal" :type))
  (should-not (nelisp-display-attribute nil :type)))

(provide 'nelisp-textprop-display-test)

;;; nelisp-textprop-display-test.el ends here
