;;; nelisp-emacs-compat-face-test.el --- ERT for face property + resolver  -*- lexical-binding: t; -*-

;; T138 / Phase 9c.1 — exercises `nelisp-emacs-compat-face' per Doc 41
;; §3.1.4 ERT gate.  Coverage targets the 7-API ship contract (§4.1)
;; plus the spec normalizer and the convenience accessors.
;;
;; Layout:
;;   §A. predicates + registry
;;   §B. spec normalize / form classifier
;;   §C. attribute merge (left-to-right)
;;   §D. :inherit chain expansion (depth 1, 4, 16, cycle)
;;   §E. unspecified preservation
;;   §F. nelisp-face-equal-p (refl / sym / trans)
;;   §G. accessors (foreground / background / attribute)
;;   §H. error paths

(require 'ert)
(require 'cl-lib)
(require 'nelisp-emacs-compat-face)

;;; ─────────────────────────────────────────────────────────────────────
;;; Test fixture: clean face registry
;;; ─────────────────────────────────────────────────────────────────────

(defmacro nelisp-face-test--with-fresh-registry (&rest body)
  "Run BODY with an empty `nelisp-face--registry'."
  (declare (indent 0) (debug (body)))
  `(let ((nelisp-face--registry (make-hash-table :test 'eq)))
     ,@body))

;;; ─────────────────────────────────────────────────────────────────────
;;; §A. predicates + registry
;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-face-define-and-lookup ()
  (nelisp-face-test--with-fresh-registry
    (should (eq (nelisp-face-define 'my-face '(:foreground "red")) 'my-face))
    (should (equal (nelisp-face-attributes 'my-face)
                   '(:foreground "red")))))

(ert-deftest nelisp-face-define-replaces-not-merges ()
  (nelisp-face-test--with-fresh-registry
    (nelisp-face-define 'f '(:foreground "red" :weight bold))
    (nelisp-face-define 'f '(:background "blue"))
    (should (equal (nelisp-face-attributes 'f) '(:background "blue")))))

(ert-deftest nelisp-face-attributes-returns-fresh-copy ()
  (nelisp-face-test--with-fresh-registry
    (nelisp-face-define 'f '(:foreground "red"))
    (let ((a (nelisp-face-attributes 'f))
          (b (nelisp-face-attributes 'f)))
      (should-not (eq a b))
      (should (equal a b))
      ;; Mutating the copy must not corrupt the registry.
      (setf (nth 1 a) "blue")
      (should (equal (nelisp-face-attributes 'f) '(:foreground "red"))))))

(ert-deftest nelisp-face-list-returns-defined-faces ()
  (nelisp-face-test--with-fresh-registry
    (nelisp-face-define 'a '(:foreground "red"))
    (nelisp-face-define 'b '(:weight bold))
    (let ((names (nelisp-face-list)))
      (should (memq 'a names))
      (should (memq 'b names))
      (should (= (length names) 2)))))

(ert-deftest nelisp-facep-detects-defined-faces ()
  (nelisp-face-test--with-fresh-registry
    (nelisp-face-define 'real-face '(:foreground "red"))
    (should (nelisp-facep 'real-face))
    (should-not (nelisp-facep 'unknown-face))
    (should-not (nelisp-facep nil))
    (should-not (nelisp-facep t))
    (should-not (nelisp-facep "string"))
    (should-not (nelisp-facep '(:foreground "red")))))

;;; ─────────────────────────────────────────────────────────────────────
;;; §B. spec normalize / form classifier
;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-face-normalize-spec-nil ()
  (should (null (nelisp-face-normalize-spec nil))))

(ert-deftest nelisp-face-normalize-spec-symbol ()
  (should (eq (nelisp-face-normalize-spec 'bold) 'bold)))

(ert-deftest nelisp-face-normalize-spec-plist ()
  (let* ((src '(:foreground "red" :weight bold))
         (out (nelisp-face-normalize-spec src)))
    (should (equal out src))
    ;; Output is a fresh copy.
    (should-not (eq out src))))

(ert-deftest nelisp-face-normalize-spec-cascade ()
  (let* ((src '(bold underline))
         (out (nelisp-face-normalize-spec src)))
    (should (equal out src))
    (should-not (eq out src))))

(ert-deftest nelisp-face-normalize-spec-bad-types-signal ()
  (should-error (nelisp-face-normalize-spec 42)
                :type 'nelisp-face-bad-spec)
  (should-error (nelisp-face-normalize-spec "string")
                :type 'nelisp-face-bad-spec))

;;; ─────────────────────────────────────────────────────────────────────
;;; §C. attribute merge (left-to-right; head wins)
;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-face-resolve-empty ()
  (should (null (nelisp-face-resolve nil)))
  (should (null (nelisp-face-resolve '()))))

(ert-deftest nelisp-face-resolve-raw-plist-passthrough ()
  (let ((out (nelisp-face-resolve '(:foreground "red" :weight bold))))
    (should (equal (plist-get out :foreground) "red"))
    (should (equal (plist-get out :weight) 'bold))))

(ert-deftest nelisp-face-resolve-symbol-via-registry ()
  (nelisp-face-test--with-fresh-registry
    (nelisp-face-define 'my '(:foreground "red"))
    (should (equal (nelisp-face-resolve 'my) '(:foreground "red")))))

(ert-deftest nelisp-face-resolve-symbol-unknown-is-nil ()
  (nelisp-face-test--with-fresh-registry
    (should (null (nelisp-face-resolve 'unknown)))))

(ert-deftest nelisp-face-resolve-cascade-left-wins ()
  (nelisp-face-test--with-fresh-registry
    (nelisp-face-define 'red '(:foreground "red"))
    (nelisp-face-define 'blue '(:foreground "blue"))
    (let ((out (nelisp-face-resolve '(red blue))))
      (should (equal (plist-get out :foreground) "red")))))

(ert-deftest nelisp-face-resolve-cascade-fills-missing ()
  (nelisp-face-test--with-fresh-registry
    (nelisp-face-define 'fg '(:foreground "red"))
    (nelisp-face-define 'bg '(:background "blue"))
    (let ((out (nelisp-face-resolve '(fg bg))))
      (should (equal (plist-get out :foreground) "red"))
      (should (equal (plist-get out :background) "blue")))))

(ert-deftest nelisp-face-resolve-cascade-mixed-symbol-and-plist ()
  (nelisp-face-test--with-fresh-registry
    (nelisp-face-define 'bold-face '(:weight bold))
    (let ((out (nelisp-face-resolve
                '((:foreground "red") bold-face))))
      (should (equal (plist-get out :foreground) "red"))
      (should (equal (plist-get out :weight) 'bold)))))

(ert-deftest nelisp-face-attribute-merge-list-only ()
  (nelisp-face-test--with-fresh-registry
    (nelisp-face-define 'b '(:weight bold))
    (let ((out (nelisp-face-attribute-merge
                '(b (:foreground "red")))))
      (should (equal (plist-get out :weight) 'bold))
      (should (equal (plist-get out :foreground) "red"))))
  (should-error (nelisp-face-attribute-merge 42)
                :type 'nelisp-face-bad-spec))

;;; ─────────────────────────────────────────────────────────────────────
;;; §D. :inherit chain expansion (depth 1, 4, 16, cycle)
;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-face-resolve-inherit-depth-1 ()
  (nelisp-face-test--with-fresh-registry
    (nelisp-face-define 'parent '(:foreground "red"))
    (nelisp-face-define 'child '(:weight bold :inherit parent))
    (let ((out (nelisp-face-resolve 'child)))
      (should (equal (plist-get out :weight) 'bold))
      (should (equal (plist-get out :foreground) "red"))
      ;; :inherit is stripped from output.
      (should-not (plist-member out :inherit)))))

(ert-deftest nelisp-face-resolve-inherit-child-overrides-parent ()
  (nelisp-face-test--with-fresh-registry
    (nelisp-face-define 'parent '(:foreground "red"))
    (nelisp-face-define 'child '(:foreground "green" :inherit parent))
    (let ((out (nelisp-face-resolve 'child)))
      (should (equal (plist-get out :foreground) "green")))))

(ert-deftest nelisp-face-resolve-inherit-depth-4 ()
  (nelisp-face-test--with-fresh-registry
    (nelisp-face-define 'a '(:foreground "red"))
    (nelisp-face-define 'b '(:weight bold :inherit a))
    (nelisp-face-define 'c '(:slant italic :inherit b))
    (nelisp-face-define 'd '(:underline t :inherit c))
    (let ((out (nelisp-face-resolve 'd)))
      (should (equal (plist-get out :underline) t))
      (should (equal (plist-get out :slant) 'italic))
      (should (equal (plist-get out :weight) 'bold))
      (should (equal (plist-get out :foreground) "red")))))

(ert-deftest nelisp-face-resolve-inherit-depth-16-truncates ()
  (nelisp-face-test--with-fresh-registry
    ;; Build a 20-deep chain: f0 inherits from f1 inherits from f2 ...
    ;; Only the first 16 should resolve; f16+ are dropped silently.
    (dotimes (i 20)
      (nelisp-face-define
       (intern (format "f%d" i))
       (if (= i 19)
           (list (intern (format ":k%d" i)) i)
         (list (intern (format ":k%d" i)) i :inherit
               (intern (format "f%d" (1+ i)))))))
    (let ((out (nelisp-face-resolve 'f0)))
      ;; Keys 0..15 should be present (depth limit = 16 = expanded
      ;; symbols), keys 16+ truncated.
      (should (plist-member out :k0))
      (should (plist-member out :k15))
      (should-not (plist-member out :k17))
      (should-not (plist-member out :k19)))))

(ert-deftest nelisp-face-resolve-inherit-cycle-truncates ()
  (nelisp-face-test--with-fresh-registry
    (nelisp-face-define 'a '(:foreground "red" :inherit b))
    (nelisp-face-define 'b '(:weight bold :inherit a))
    (let ((out (nelisp-face-resolve 'a)))
      ;; Cycle guard: a's own attrs win, b contributes once, then stop.
      (should (equal (plist-get out :foreground) "red"))
      (should (equal (plist-get out :weight) 'bold))
      (should-not (plist-member out :inherit)))))

(ert-deftest nelisp-face-resolve-inherit-list-of-faces ()
  (nelisp-face-test--with-fresh-registry
    (nelisp-face-define 'a '(:foreground "red"))
    (nelisp-face-define 'b '(:background "blue"))
    (nelisp-face-define 'c '(:weight bold :inherit (a b)))
    (let ((out (nelisp-face-resolve 'c)))
      (should (equal (plist-get out :weight) 'bold))
      (should (equal (plist-get out :foreground) "red"))
      (should (equal (plist-get out :background) "blue")))))

;;; ─────────────────────────────────────────────────────────────────────
;;; §E. unspecified preservation
;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-face-resolve-keeps-unspecified ()
  (nelisp-face-test--with-fresh-registry
    (nelisp-face-define 'f '(:foreground unspecified :weight bold))
    (let ((out (nelisp-face-resolve 'f)))
      (should (eq (plist-get out :foreground) 'unspecified))
      (should (eq (plist-get out :weight) 'bold)))))

(ert-deftest nelisp-face-unspecified-shadows-parent ()
  ;; Per Doc 41 §2.3: `unspecified' values are KEPT.  In the
  ;; left-to-right model an explicit `unspecified' on the child still
  ;; wins over the parent's concrete value (the backend decides
  ;; fallback semantics).
  (nelisp-face-test--with-fresh-registry
    (nelisp-face-define 'parent '(:foreground "red"))
    (nelisp-face-define 'child '(:foreground unspecified :inherit parent))
    (let ((out (nelisp-face-resolve 'child)))
      (should (eq (plist-get out :foreground) 'unspecified)))))

;;; ─────────────────────────────────────────────────────────────────────
;;; §F. nelisp-face-equal-p (refl / sym / trans)
;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-face-equal-p-reflexive ()
  (nelisp-face-test--with-fresh-registry
    (nelisp-face-define 'f '(:foreground "red" :weight bold))
    (should (nelisp-face-equal-p 'f 'f))
    (should (nelisp-face-equal-p '(:foreground "red")
                                 '(:foreground "red")))
    (should (nelisp-face-equal-p nil nil))))

(ert-deftest nelisp-face-equal-p-symmetric ()
  (nelisp-face-test--with-fresh-registry
    (nelisp-face-define 'a '(:foreground "red"))
    (should (nelisp-face-equal-p 'a '(:foreground "red")))
    (should (nelisp-face-equal-p '(:foreground "red") 'a))))

(ert-deftest nelisp-face-equal-p-transitive ()
  (nelisp-face-test--with-fresh-registry
    (nelisp-face-define 'a '(:foreground "red"))
    (nelisp-face-define 'b '(:foreground "red"))
    (let ((c '(:foreground "red")))
      (should (nelisp-face-equal-p 'a 'b))
      (should (nelisp-face-equal-p 'b c))
      (should (nelisp-face-equal-p 'a c)))))

(ert-deftest nelisp-face-equal-p-key-order-insensitive ()
  (should (nelisp-face-equal-p
           '(:foreground "red" :weight bold)
           '(:weight bold :foreground "red"))))

(ert-deftest nelisp-face-equal-p-detects-difference ()
  (should-not (nelisp-face-equal-p
               '(:foreground "red")
               '(:foreground "blue")))
  (should-not (nelisp-face-equal-p
               '(:foreground "red")
               '(:foreground "red" :weight bold))))

;;; ─────────────────────────────────────────────────────────────────────
;;; §G. accessors (foreground / background / attribute)
;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-face-foreground-resolves ()
  (nelisp-face-test--with-fresh-registry
    (nelisp-face-define 'f '(:foreground "red"))
    (should (equal (nelisp-face-foreground 'f) "red"))
    (should (equal (nelisp-face-foreground '(:foreground "blue")) "blue"))
    (should (null (nelisp-face-foreground nil)))))

(ert-deftest nelisp-face-background-resolves ()
  (nelisp-face-test--with-fresh-registry
    (nelisp-face-define 'f '(:background "yellow"))
    (should (equal (nelisp-face-background 'f) "yellow"))
    (should (null (nelisp-face-background '(:foreground "red"))))))

(ert-deftest nelisp-face-attribute-arbitrary-key ()
  (nelisp-face-test--with-fresh-registry
    (nelisp-face-define 'f '(:weight bold :slant italic))
    (should (eq (nelisp-face-attribute 'f :weight) 'bold))
    (should (eq (nelisp-face-attribute 'f :slant) 'italic))
    (should (null (nelisp-face-attribute 'f :foreground)))))

(ert-deftest nelisp-face-attribute-rejects-non-keyword ()
  (should-error (nelisp-face-attribute 'foo 'weight)
                :type 'nelisp-face-bad-attribute))

;;; ─────────────────────────────────────────────────────────────────────
;;; §H. error paths
;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-face-define-rejects-bad-name ()
  (should-error (nelisp-face-define nil '(:foreground "red"))
                :type 'nelisp-face-bad-name)
  (should-error (nelisp-face-define t '(:foreground "red"))
                :type 'nelisp-face-bad-name)
  (should-error (nelisp-face-define "string" '(:foreground "red"))
                :type 'nelisp-face-bad-name))

(ert-deftest nelisp-face-define-rejects-bad-attr-plist ()
  (should-error (nelisp-face-define 'f '(:foreground))
                :type 'nelisp-face-bad-attribute)
  (should-error (nelisp-face-define 'f '(foreground "red"))
                :type 'nelisp-face-bad-attribute))

(ert-deftest nelisp-face-resolve-rejects-bad-spec ()
  (should-error (nelisp-face-resolve 42)
                :type 'nelisp-face-bad-spec)
  (should-error (nelisp-face-resolve "string")
                :type 'nelisp-face-bad-spec))

(ert-deftest nelisp-face-contract-version-locked ()
  ;; Doc 41 §4.1 LOCKED v2: contract version is 1.  Bumping it requires
  ;; a Doc 41 LOCKED revision; this guard catches accidental changes.
  (should (= nelisp-face-property-contract-version 1)))

;;; ─────────────────────────────────────────────────────────────────────
;;; §I. round-trip via (notional) put-text-property face value
;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-face-put-text-property-3-spec-forms-roundtrip ()
  ;; Per Doc 41 §3.1.1: `put-text-property` (face) must accept the 3
  ;; forms; `nelisp-face-normalize-spec' is what callers use to
  ;; validate before storage.  Round-trip = normalize then resolve
  ;; gives the same flat plist as resolving the original.
  (nelisp-face-test--with-fresh-registry
    (nelisp-face-define 'parent '(:foreground "red"))
    (dolist (spec '(nil
                    parent
                    (parent (:weight bold))
                    (:foreground "blue" :inherit parent)))
      (should (equal (nelisp-face-resolve (nelisp-face-normalize-spec spec))
                     (nelisp-face-resolve spec))))))

(provide 'nelisp-emacs-compat-face-test)

;;; nelisp-emacs-compat-face-test.el ends here
