;;; emacs-buffer-test.el --- ERT tests for emacs-buffer.el  -*- lexical-binding: t; -*-

;; Phase 1 module 1/6 tests per nelisp-emacs Doc 01.
;; Covers all 6 categories of `emacs-buffer-*' API:
;;   A. buffer registry/local variables (11 tests)
;;   B. text-property/property query    (26 tests)
;;   C. undo system              (6 tests)
;;   D. modification tracking    (6 tests)
;;   E. additional buffer ops    (5 tests)
;;   F. overlay                  (18 tests)

(require 'ert)
(require 'emacs-buffer)

;;; Fresh-world fixture (resets every global in both modules)

(defmacro emacs-buffer-test--with-fresh-world (&rest body)
  "Run BODY with clean nelisp-ec + emacs-buffer state."
  (declare (indent 0) (debug (body)))
  `(let ((nelisp-ec--buffers nil)
         (nelisp-ec--current-buffer nil)
         (emacs-buffer--state (make-hash-table :test 'eq))
         (emacs-buffer--variable-buffer-local nil)
         (emacs-buffer--default-values (make-hash-table :test 'eq))
         (emacs-buffer--overlay-counter 0))
     ,@body))

;;;; A. buffer registry/local variables (11 tests)

(ert-deftest emacs-buffer-current-public-wrapper ()
  (emacs-buffer-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "x")))
      (nelisp-ec-set-buffer b)
      (should (eq b (emacs-buffer-current))))))

(ert-deftest emacs-buffer-make-local-variable-creates-binding ()
  (emacs-buffer-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "x")))
      (nelisp-ec-set-buffer b)
      (emacs-buffer-set-default 'foo 42)
      (emacs-buffer-make-local-variable 'foo)
      (should (emacs-buffer-local-variable-p 'foo))
      (should (= 42 (emacs-buffer-buffer-local-value 'foo b))))))

(ert-deftest emacs-buffer-make-local-variable-idempotent ()
  (emacs-buffer-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "x")))
      (nelisp-ec-set-buffer b)
      (emacs-buffer-set-default 'foo 1)
      (emacs-buffer-make-local-variable 'foo)
      (emacs-buffer-set-buffer-local-value 'foo b 99)
      ;; second call must NOT clobber the local value
      (emacs-buffer-make-local-variable 'foo)
      (should (= 99 (emacs-buffer-buffer-local-value 'foo b))))))

(ert-deftest emacs-buffer-buffer-local-value-falls-back-to-default ()
  (emacs-buffer-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "x")))
      (emacs-buffer-set-default 'bar "default")
      (should (string-equal "default"
                            (emacs-buffer-buffer-local-value 'bar b))))))

(ert-deftest emacs-buffer-buffer-local-value-void-when-no-default ()
  (emacs-buffer-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "x")))
      (should-error (emacs-buffer-buffer-local-value 'absent b)
                    :type 'void-variable))))

(ert-deftest emacs-buffer-default-value-and-default-boundp ()
  (emacs-buffer-test--with-fresh-world
    (should (null (emacs-buffer-default-boundp 'absent)))
    (should-error (emacs-buffer-default-value 'absent) :type 'void-variable)
    (emacs-buffer-set-default 'present 7)
    (should (emacs-buffer-default-boundp 'present))
    (should (= 7 (emacs-buffer-default-value 'present)))))

(ert-deftest emacs-buffer-setq-default-macro-works ()
  (emacs-buffer-test--with-fresh-world
    (emacs-buffer-setq-default xxx 123)
    (should (= 123 (emacs-buffer-default-value 'xxx)))))

(ert-deftest emacs-buffer-local-variable-if-set-p ()
  (emacs-buffer-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "x")))
      (nelisp-ec-set-buffer b)
      (should-not (emacs-buffer-local-variable-if-set-p 'mode-line-format))
      (emacs-buffer-make-variable-buffer-local 'mode-line-format)
      (should (emacs-buffer-local-variable-if-set-p 'mode-line-format))
      ;; locally-bound vars also count
      (emacs-buffer-set-default 'foo 1)
      (emacs-buffer-make-local-variable 'foo)
      (should (emacs-buffer-local-variable-if-set-p 'foo)))))

(ert-deftest emacs-buffer-buffer-local-variables-lists-all ()
  (emacs-buffer-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "x")))
      (nelisp-ec-set-buffer b)
      (emacs-buffer-set-default 'a 1)
      (emacs-buffer-set-default 'b 2)
      (emacs-buffer-make-local-variable 'a)
      (emacs-buffer-make-local-variable 'b)
      (let ((alist (emacs-buffer-buffer-local-variables b)))
        (should (= 2 (length alist)))
        (should (assq 'a alist))
        (should (assq 'b alist))))))

(ert-deftest emacs-buffer-kill-local-variable-removes-binding ()
  (emacs-buffer-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "x")))
      (nelisp-ec-set-buffer b)
      (emacs-buffer-set-default 'foo 0)
      (emacs-buffer-make-local-variable 'foo)
      (should (emacs-buffer-local-variable-p 'foo))
      (emacs-buffer-kill-local-variable 'foo)
      (should-not (emacs-buffer-local-variable-p 'foo)))))

(ert-deftest emacs-buffer-kill-all-local-variables-clears-all ()
  (emacs-buffer-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "x")))
      (nelisp-ec-set-buffer b)
      (emacs-buffer-set-default 'a 1)
      (emacs-buffer-set-default 'b 2)
      (emacs-buffer-make-local-variable 'a)
      (emacs-buffer-make-local-variable 'b)
      (emacs-buffer-kill-all-local-variables)
      (should (null (emacs-buffer-buffer-local-variables b))))))

;;;; B. text-property/property query (26 tests)

(ert-deftest emacs-buffer-put-and-get-text-property ()
  (emacs-buffer-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "tp")))
      (nelisp-ec-set-buffer b)
      (nelisp-ec-insert "hello world")
      (emacs-buffer-put-text-property 1 6 'face 'bold)
      (should (eq 'bold (emacs-buffer-get-text-property 1 'face)))
      (should (eq 'bold (emacs-buffer-get-text-property 5 'face)))
      ;; outside range
      (should (null (emacs-buffer-get-text-property 6 'face)))
      (should (null (emacs-buffer-get-text-property 11 'face))))))

(ert-deftest emacs-buffer-put-text-property-overwrites ()
  (emacs-buffer-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "tp")))
      (nelisp-ec-set-buffer b)
      (nelisp-ec-insert "abcdef")
      (emacs-buffer-put-text-property 1 7 'face 'bold)
      (emacs-buffer-put-text-property 3 5 'face 'italic)
      (should (eq 'bold (emacs-buffer-get-text-property 1 'face)))
      (should (eq 'italic (emacs-buffer-get-text-property 3 'face)))
      (should (eq 'italic (emacs-buffer-get-text-property 4 'face)))
      (should (eq 'bold (emacs-buffer-get-text-property 5 'face)))
      (should (eq 'bold (emacs-buffer-get-text-property 6 'face))))))

(ert-deftest emacs-buffer-add-text-properties-merges ()
  (emacs-buffer-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "tp")))
      (nelisp-ec-set-buffer b)
      (nelisp-ec-insert "hello")
      (emacs-buffer-add-text-properties 1 6 '(face bold))
      (emacs-buffer-add-text-properties 1 6 '(weight 700))
      (should (eq 'bold (emacs-buffer-get-text-property 1 'face)))
      (should (= 700 (emacs-buffer-get-text-property 1 'weight))))))

(ert-deftest emacs-buffer-remove-text-properties-drops-keys ()
  (emacs-buffer-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "tp")))
      (nelisp-ec-set-buffer b)
      (nelisp-ec-insert "hello")
      (emacs-buffer-add-text-properties 1 6 '(face bold weight 700))
      (emacs-buffer-remove-text-properties 1 6 '(face))
      (should (null (emacs-buffer-get-text-property 1 'face)))
      (should (= 700 (emacs-buffer-get-text-property 1 'weight))))))

(ert-deftest emacs-buffer-remove-text-properties-accepts-plist ()
  (emacs-buffer-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "tp")))
      (nelisp-ec-set-buffer b)
      (nelisp-ec-insert "hello")
      (emacs-buffer-add-text-properties 1 6 '(face bold weight 700))
      ;; Emacs-style plist (PROP VALUE PROP VALUE ...) interpreted as
      ;; "drop these property names regardless of values"
      (emacs-buffer-remove-text-properties 1 6 '(face nil weight nil))
      (should (null (emacs-buffer-get-text-property 1 'face)))
      (should (null (emacs-buffer-get-text-property 1 'weight))))))

(ert-deftest emacs-buffer-text-property-at-returns-plist ()
  (emacs-buffer-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "tp")))
      (nelisp-ec-set-buffer b)
      (nelisp-ec-insert "hello")
      (emacs-buffer-add-text-properties 1 6 '(face bold weight 700))
      (let ((pl (emacs-buffer-text-property-at 1)))
        (should (equal 'bold (plist-get pl 'face)))
        (should (= 700 (plist-get pl 'weight)))))))

(ert-deftest emacs-buffer-text-property-view-clips-and-filters ()
  (emacs-buffer-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "tp")))
      (nelisp-ec-set-buffer b)
      (nelisp-ec-insert "abcdef")
      (emacs-buffer-add-text-properties 2 6 '(face bold weight 700))
      (should (equal '((3 5 (face bold)))
                     (emacs-buffer-text-property-view 3 5 '(face) b)))
      (should (equal '((2 6 (face bold weight 700)))
                     (emacs-buffer-text-property-view 1 7 nil b))))))

(ert-deftest emacs-buffer-text-property-view-resolves-category ()
  (emacs-buffer-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "tp"))
          (cat (make-symbol "category")))
      (put cat 'face 'category-face)
      (nelisp-ec-set-buffer b)
      (nelisp-ec-insert "abc")
      (emacs-buffer-add-text-properties 1 3 (list 'category cat))
      (should (equal '((1 3 (face category-face)))
                     (emacs-buffer-text-property-view 1 3 '(face) b))))))

(ert-deftest emacs-buffer-put-text-property-rejects-empty-range ()
  (emacs-buffer-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "tp")))
      (nelisp-ec-set-buffer b)
      (nelisp-ec-insert "x")
      (should-error (emacs-buffer-put-text-property 1 1 'face 'bold)
                    :type 'nelisp-ec-args-out-of-range))))

;;;; B'. text-property advanced (14 tests)

(ert-deftest emacs-buffer-set-text-properties-replaces-plist ()
  (emacs-buffer-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "tp")))
      (nelisp-ec-set-buffer b)
      (nelisp-ec-insert "hello")
      ;; Add (face bold, weight 700).
      (emacs-buffer-add-text-properties 1 6 '(face bold weight 700))
      ;; Replace with just (face italic).
      (emacs-buffer-set-text-properties 1 6 '(face italic))
      (should (eq 'italic (emacs-buffer-get-text-property 3 'face)))
      (should-not (emacs-buffer-get-text-property 3 'weight)))))

(ert-deftest emacs-buffer-set-text-properties-nil-clears ()
  (emacs-buffer-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "tp")))
      (nelisp-ec-set-buffer b)
      (nelisp-ec-insert "hello")
      (emacs-buffer-add-text-properties 1 6 '(face bold))
      (emacs-buffer-set-text-properties 1 6 nil)
      (should-not (emacs-buffer-get-text-property 3 'face))
      (should-not (emacs-buffer-text-property-at 3)))))

(ert-deftest emacs-buffer-set-text-properties-rejects-bad-plist ()
  (emacs-buffer-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "tp")))
      (nelisp-ec-set-buffer b)
      (nelisp-ec-insert "x")
      (should-error (emacs-buffer-set-text-properties 1 2 '(face))
                    :type 'wrong-type-argument))))

(ert-deftest emacs-buffer-get-text-property-category-inheritance ()
  ;; Category symbol carries face=red.  Range has only (category cat).
  (let ((cat (make-symbol "test-cat")))
    (put cat 'face 'red)
    (put cat 'priority 5)
    (emacs-buffer-test--with-fresh-world
      (let ((b (nelisp-ec-generate-new-buffer "tp")))
        (nelisp-ec-set-buffer b)
        (nelisp-ec-insert "hello")
        (emacs-buffer-add-text-properties 1 6 (list 'category cat))
        ;; Inherited via category.
        (should (eq 'red (emacs-buffer-get-text-property 3 'face)))
        (should (eq 5 (emacs-buffer-get-text-property 3 'priority)))
        ;; Not on category either → nil.
        (should-not (emacs-buffer-get-text-property 3 'no-such))))))

(ert-deftest emacs-buffer-get-text-property-direct-overrides-category ()
  (let ((cat (make-symbol "test-cat")))
    (put cat 'face 'red)
    (emacs-buffer-test--with-fresh-world
      (let ((b (nelisp-ec-generate-new-buffer "tp")))
        (nelisp-ec-set-buffer b)
        (nelisp-ec-insert "hello")
        (emacs-buffer-add-text-properties 1 6 (list 'category cat
                                                    'face 'blue))
        ;; Direct (face blue) wins over category (face red).
        (should (eq 'blue (emacs-buffer-get-text-property 3 'face)))))))

(ert-deftest emacs-buffer-get-text-property-category-non-symbol-ignored ()
  (emacs-buffer-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "tp")))
      (nelisp-ec-set-buffer b)
      (nelisp-ec-insert "hi")
      ;; category set to a non-symbol → no inheritance.
      (emacs-buffer-add-text-properties 1 3 '(category "not-a-symbol"))
      (should-not (emacs-buffer-get-text-property 1 'face)))))

(ert-deftest emacs-buffer-next-property-change-inside-interval ()
  (emacs-buffer-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "tp")))
      (nelisp-ec-set-buffer b)
      (nelisp-ec-insert "hello world")
      (emacs-buffer-put-text-property 3 7 'face 'bold)
      ;; POS=4 inside [3,7) — next change at 7.
      (should (= 7 (emacs-buffer-next-property-change 4))))))

(ert-deftest emacs-buffer-next-property-change-before-first-interval ()
  (emacs-buffer-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "tp")))
      (nelisp-ec-set-buffer b)
      (nelisp-ec-insert "hello world")
      (emacs-buffer-put-text-property 5 9 'face 'bold)
      ;; POS=1 has no props — next change at start of first interval (5).
      (should (= 5 (emacs-buffer-next-property-change 1))))))

(ert-deftest emacs-buffer-next-property-change-no-more ()
  (emacs-buffer-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "tp")))
      (nelisp-ec-set-buffer b)
      (nelisp-ec-insert "hello")
      ;; No intervals at all.
      (should-not (emacs-buffer-next-property-change 2))
      ;; With LIMIT → returns LIMIT.
      (should (= 10 (emacs-buffer-next-property-change 2 nil 10))))))

(ert-deftest emacs-buffer-previous-property-change-basic ()
  (emacs-buffer-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "tp")))
      (nelisp-ec-set-buffer b)
      (nelisp-ec-insert "hello world")
      (emacs-buffer-put-text-property 3 7 'face 'bold)
      ;; POS=9 — last change before is at 7 (end of interval).
      (should (= 7 (emacs-buffer-previous-property-change 9)))
      ;; POS=5 inside — previous change at 3 (start of interval).
      (should (= 3 (emacs-buffer-previous-property-change 5))))))

(ert-deftest emacs-buffer-next-single-property-change-finds-change ()
  (emacs-buffer-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "tp")))
      (nelisp-ec-set-buffer b)
      (nelisp-ec-insert "hello world")
      ;; [3,7) face=bold, [7,9) face=italic.
      (emacs-buffer-put-text-property 3 7 'face 'bold)
      (emacs-buffer-put-text-property 7 9 'face 'italic)
      ;; POS=4 (face=bold) — next change in face at 7.
      (should (= 7 (emacs-buffer-next-single-property-change 4 'face))))))

(ert-deftest emacs-buffer-next-single-property-change-skips-unrelated ()
  (emacs-buffer-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "tp")))
      (nelisp-ec-set-buffer b)
      (nelisp-ec-insert "hello world")
      ;; [3,5) face=bold weight=700, [5,7) face=bold weight=400.
      (emacs-buffer-add-text-properties 3 5 '(face bold weight 700))
      (emacs-buffer-add-text-properties 5 7 '(face bold weight 400))
      ;; face is constant across [3,7) — next change at 7 (= where face
      ;; goes back to nil) honouring LIMIT 100.
      (should (= 7 (emacs-buffer-next-single-property-change 3 'face nil 100))))))

(ert-deftest emacs-buffer-previous-single-property-change-finds-change ()
  (emacs-buffer-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "tp")))
      (nelisp-ec-set-buffer b)
      (nelisp-ec-insert "hello world")
      (emacs-buffer-put-text-property 3 7 'face 'bold)
      ;; POS=9 face=nil — previous change in face at 7 (where it was bold).
      (should (= 7 (emacs-buffer-previous-single-property-change 9 'face))))))

(ert-deftest emacs-buffer-get-char-property-overlay-wins ()
  (emacs-buffer-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "tp")))
      (nelisp-ec-set-buffer b)
      (nelisp-ec-insert "hello")
      (emacs-buffer-put-text-property 1 6 'face 'red)
      (let ((ov (emacs-buffer-make-overlay 1 6 b)))
        (emacs-buffer-overlay-put ov 'face 'blue)
        ;; Overlay value wins over text-property.
        (should (eq 'blue (emacs-buffer-get-char-property 3 'face b)))
        ;; Property not on overlay falls through.
        (emacs-buffer-overlay-put ov 'weight 700)
        (should (eq 700 (emacs-buffer-get-char-property 3 'weight b)))))))

(ert-deftest emacs-buffer-get-char-property-overlay-priority-wins ()
  (emacs-buffer-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "tp")))
      (nelisp-ec-set-buffer b)
      (nelisp-ec-insert "hello")
      (let ((low (emacs-buffer-make-overlay 1 6 b))
            (high (emacs-buffer-make-overlay 1 6 b)))
        (emacs-buffer-overlay-put low 'face 'low)
        (emacs-buffer-overlay-put low 'priority 1)
        (emacs-buffer-overlay-put high 'face 'high)
        (emacs-buffer-overlay-put high 'priority 10)
        (should (eq 'high (emacs-buffer-get-char-property 3 'face b)))))))

(ert-deftest emacs-buffer-get-char-property-overlay-insertion-tie-wins ()
  (emacs-buffer-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "tp")))
      (nelisp-ec-set-buffer b)
      (nelisp-ec-insert "hello")
      (let ((first (emacs-buffer-make-overlay 1 6 b))
            (second (emacs-buffer-make-overlay 1 6 b)))
        (emacs-buffer-overlay-put first 'face 'first)
        (emacs-buffer-overlay-put second 'face 'second)
        (should (eq 'second (emacs-buffer-get-char-property 3 'face b)))))))

(ert-deftest emacs-buffer-get-char-property-text-prop-fallback ()
  (emacs-buffer-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "tp")))
      (nelisp-ec-set-buffer b)
      (nelisp-ec-insert "hello")
      ;; No overlays — falls through to text-property (with category).
      (let ((cat (make-symbol "tp-cat")))
        (put cat 'face 'cyan)
        (emacs-buffer-add-text-properties 1 6 (list 'category cat))
        (should (eq 'cyan (emacs-buffer-get-char-property 3 'face b)))))))

;;;; C. undo system (6 tests)

(ert-deftest emacs-buffer-undo-list-defaults-empty ()
  (emacs-buffer-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "u")))
      (should (null (emacs-buffer-buffer-undo-list b))))))

(ert-deftest emacs-buffer-buffer-disable-and-enable-undo ()
  (emacs-buffer-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "u")))
      (emacs-buffer-buffer-disable-undo b)
      (should (eq t (emacs-buffer-buffer-undo-list b)))
      (emacs-buffer-buffer-enable-undo b)
      (should (null (emacs-buffer-buffer-undo-list b))))))

(ert-deftest emacs-buffer-undo-boundary-pushes-nil ()
  (emacs-buffer-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "u")))
      (nelisp-ec-set-buffer b)
      (emacs-buffer-undo-boundary)
      (should (equal '(nil) (emacs-buffer-buffer-undo-list b))))))

(ert-deftest emacs-buffer-undo-roundtrip-insertion ()
  (emacs-buffer-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "u")))
      (nelisp-ec-set-buffer b)
      (nelisp-ec-insert "hello")
      (emacs-buffer-record-insertion 1 6 b)
      (should (equal "hello" (nelisp-ec-buffer-string)))
      (emacs-buffer-undo b)
      (should (string-empty-p (nelisp-ec-buffer-string))))))

(ert-deftest emacs-buffer-undo-roundtrip-deletion ()
  (emacs-buffer-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "u")))
      (nelisp-ec-set-buffer b)
      (nelisp-ec-insert "hello")
      ;; simulate a deletion of "ell" at position 2
      (let ((deleted (nelisp-ec-buffer-substring 2 5)))
        (nelisp-ec-delete-region 2 5)
        (emacs-buffer-record-deletion deleted 2 b))
      (should (equal "ho" (nelisp-ec-buffer-string)))
      (emacs-buffer-undo b)
      (should (equal "hello" (nelisp-ec-buffer-string))))))

(ert-deftest emacs-buffer-undo-disabled-signals ()
  (emacs-buffer-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "u")))
      (emacs-buffer-buffer-disable-undo b)
      (should-error (emacs-buffer-undo b)
                    :type 'emacs-buffer-undo-disabled))))

;;;; D. modification tracking (6 tests)

(ert-deftest emacs-buffer-modified-p-tracks-edits ()
  (emacs-buffer-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "m")))
      (nelisp-ec-set-buffer b)
      (should-not (emacs-buffer-buffer-modified-p))
      (nelisp-ec-insert "x")
      (should (emacs-buffer-buffer-modified-p))
      (emacs-buffer-set-buffer-modified-p nil)
      (should-not (emacs-buffer-buffer-modified-p)))))

(ert-deftest emacs-buffer-restore-buffer-modified-p ()
  (emacs-buffer-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "m")))
      (nelisp-ec-set-buffer b)
      (emacs-buffer-restore-buffer-modified-p t)
      (should (emacs-buffer-buffer-modified-p))
      (emacs-buffer-restore-buffer-modified-p nil)
      (should-not (emacs-buffer-buffer-modified-p)))))

(ert-deftest emacs-buffer-toggle-read-only-direct ()
  (emacs-buffer-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "m")))
      (nelisp-ec-set-buffer b)
      (let ((result (emacs-buffer-toggle-read-only-direct b)))
        (should (plist-get result :read-only))
        (should (equal "buffer-read-only: on"
                       (plist-get result :message)))
        (nelisp-ec-with-current-buffer b
          (should buffer-read-only)))
      (let ((result (emacs-buffer-toggle-read-only-direct b)))
        (should-not (plist-get result :read-only))
        (should (equal "buffer-read-only: off"
                       (plist-get result :message)))
        (nelisp-ec-with-current-buffer b
          (should-not buffer-read-only))))))

(ert-deftest emacs-buffer-chars-modified-tick-monotonic ()
  (emacs-buffer-test--with-fresh-world
    (let* ((b (nelisp-ec-generate-new-buffer "m"))
           (t0 (emacs-buffer-buffer-chars-modified-tick b)))
      (emacs-buffer-bump-modified-tick b)
      (emacs-buffer-bump-modified-tick b)
      (should (= (+ t0 2) (emacs-buffer-buffer-chars-modified-tick b))))))

(ert-deftest emacs-buffer-text-tick-tracks-text-only ()
  (emacs-buffer-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "m")))
      (nelisp-ec-set-buffer b)
      (let ((t0 (emacs-buffer-buffer-text-tick b)))
        (nelisp-ec-insert "abc")
        (should (> (emacs-buffer-buffer-text-tick b) t0))
        (let ((t1 (emacs-buffer-buffer-text-tick b)))
          (emacs-buffer-put-text-property 1 2 'face 'bold b)
          (should (= t1 (emacs-buffer-buffer-text-tick b)))
          (nelisp-ec-delete-region 1 2)
          (should (> (emacs-buffer-buffer-text-tick b) t1)))))))

(ert-deftest emacs-buffer-modify-without-undo-restores-on-error ()
  (emacs-buffer-test--with-fresh-world
    (let* ((b (nelisp-ec-generate-new-buffer "m"))
           (ext (emacs-buffer--ensure-ext b)))
      (nelisp-ec-set-buffer b)
      (emacs-buffer-buffer-enable-undo b)  ;; nil
      (ignore-errors
        (emacs-buffer-modify-without-undo
          (should (eq t (emacs-buffer--ext-undo-list ext)))
          (error "boom")))
      ;; undo-list must be restored
      (should (null (emacs-buffer--ext-undo-list ext))))))

;;;; E. additional buffer ops (5 tests)

(ert-deftest emacs-buffer-clone-indirect-buffer-shares-text ()
  (emacs-buffer-test--with-fresh-world
    (let ((base (nelisp-ec-generate-new-buffer "base")))
      (nelisp-ec-set-buffer base)
      (nelisp-ec-insert "shared")
      (let ((clone (emacs-buffer-clone-indirect-buffer "clone" base)))
        (should (eq base (emacs-buffer-buffer-base-buffer clone)))
        (nelisp-ec-set-buffer clone)
        (should (equal "shared" (nelisp-ec-buffer-string)))
        ;; mutate via base, see in clone
        (nelisp-ec-set-buffer base)
        (nelisp-ec-goto-char (1+ (length "shared")))
        (nelisp-ec-insert "!")
        (nelisp-ec-set-buffer clone)
        (should (equal "shared!" (nelisp-ec-buffer-string)))))))

(ert-deftest emacs-buffer-buffer-base-buffer-nil-for-direct ()
  (emacs-buffer-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "direct")))
      (should (null (emacs-buffer-buffer-base-buffer b))))))

(ert-deftest emacs-buffer-buffer-list-returns-all ()
  (emacs-buffer-test--with-fresh-world
    (let ((a (nelisp-ec-generate-new-buffer "a"))
          (b (nelisp-ec-generate-new-buffer "b")))
      (let ((all (emacs-buffer-buffer-list)))
        (should (= 2 (length all)))
        (should (memq a all))
        (should (memq b all))))))

(ert-deftest emacs-buffer-buffers-by-mode-filters ()
  (emacs-buffer-test--with-fresh-world
    (let ((a (nelisp-ec-generate-new-buffer "a"))
          (b (nelisp-ec-generate-new-buffer "b")))
      (emacs-buffer-set-default 'major-mode 'text-mode)
      (emacs-buffer-make-local-variable 'major-mode a)
      (emacs-buffer-set-buffer-local-value 'major-mode a 'lisp-mode)
      (emacs-buffer-make-local-variable 'major-mode b)
      (emacs-buffer-set-buffer-local-value 'major-mode b 'text-mode)
      (let ((lisp (emacs-buffer-buffers-by-mode
                   (lambda (buf)
                     (eq 'lisp-mode
                         (emacs-buffer-buffer-local-value 'major-mode buf))))))
        (should (= 1 (length lisp)))
        (should (memq a lisp))))))

(ert-deftest emacs-buffer-generate-new-buffer-name-uniquifies ()
  (emacs-buffer-test--with-fresh-world
    (should (equal "scratch" (emacs-buffer-generate-new-buffer-name "scratch")))
    (nelisp-ec-generate-new-buffer "scratch")
    (should (equal "scratch<2>" (emacs-buffer-generate-new-buffer-name "scratch")))
    (nelisp-ec-generate-new-buffer "scratch")
    (should (equal "scratch<3>" (emacs-buffer-generate-new-buffer-name "scratch")))))

;;;; F. overlay (18 tests)

(ert-deftest emacs-buffer-overlay-make-and-accessors ()
  (emacs-buffer-test--with-fresh-world
    (let* ((b (nelisp-ec-generate-new-buffer "x"))
           (ov (emacs-buffer-make-overlay 3 7 b)))
      (should (emacs-buffer-overlayp ov))
      (should (= 3 (emacs-buffer-overlay-start ov)))
      (should (= 7 (emacs-buffer-overlay-end ov)))
      (should (eq b (emacs-buffer-overlay-buffer ov))))))

(ert-deftest emacs-buffer-overlay-make-swaps-reversed-range ()
  (emacs-buffer-test--with-fresh-world
    (let* ((b (nelisp-ec-generate-new-buffer "x"))
           (ov (emacs-buffer-make-overlay 9 4 b)))
      (should (= 4 (emacs-buffer-overlay-start ov)))
      (should (= 9 (emacs-buffer-overlay-end ov))))))

(ert-deftest emacs-buffer-overlay-make-rejects-non-integer-range ()
  (emacs-buffer-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "x")))
      (should-error (emacs-buffer-make-overlay "x" 5 b)
                    :type 'wrong-type-argument))))

(ert-deftest emacs-buffer-overlayp-rejects-non-overlay ()
  (should-not (emacs-buffer-overlayp 42))
  (should-not (emacs-buffer-overlayp '(1 2 3)))
  (should-not (emacs-buffer-overlayp "ov")))

(ert-deftest emacs-buffer-overlay-put-get-roundtrip ()
  (emacs-buffer-test--with-fresh-world
    (let* ((b (nelisp-ec-generate-new-buffer "x"))
           (ov (emacs-buffer-make-overlay 1 5 b)))
      (should (eq 'red (emacs-buffer-overlay-put ov 'face 'red)))
      (should (eq 'red (emacs-buffer-overlay-get ov 'face)))
      ;; absent key -> nil
      (should-not (emacs-buffer-overlay-get ov 'no-such-prop)))))

(ert-deftest emacs-buffer-overlay-properties-returns-copy ()
  (emacs-buffer-test--with-fresh-world
    (let* ((b (nelisp-ec-generate-new-buffer "x"))
           (ov (emacs-buffer-make-overlay 1 5 b)))
      (emacs-buffer-overlay-put ov 'face 'red)
      (emacs-buffer-overlay-put ov 'priority 1)
      (let ((props (emacs-buffer-overlay-properties ov)))
        (should (equal 'red (plist-get props 'face)))
        (should (equal 1 (plist-get props 'priority)))
        ;; mutating the copy must not affect OV
        (plist-put props 'face 'blue)
        (should (eq 'red (emacs-buffer-overlay-get ov 'face)))))))

(ert-deftest emacs-buffer-overlays-at-single ()
  (emacs-buffer-test--with-fresh-world
    (let* ((b (nelisp-ec-generate-new-buffer "x"))
           (ov (emacs-buffer-make-overlay 3 7 b)))
      ;; START is inclusive, END is exclusive.
      (should (equal (list ov) (emacs-buffer-overlays-at 3 b)))
      (should (equal (list ov) (emacs-buffer-overlays-at 6 b)))
      ;; END boundary is NOT included.
      (should-not (emacs-buffer-overlays-at 7 b))
      ;; Outside the range.
      (should-not (emacs-buffer-overlays-at 2 b)))))

(ert-deftest emacs-buffer-overlays-at-multiple-ordered-by-start ()
  (emacs-buffer-test--with-fresh-world
    (let* ((b (nelisp-ec-generate-new-buffer "x"))
           (o1 (emacs-buffer-make-overlay 5 10 b))
           (o2 (emacs-buffer-make-overlay 3 8  b))
           (o3 (emacs-buffer-make-overlay 7 9  b)))
      ;; Position 7 is in all three.  Order = ascending START
      ;; (3, 5, 7) -> (o2 o1 o3).
      (should (equal (list o2 o1 o3) (emacs-buffer-overlays-at 7 b))))))

(ert-deftest emacs-buffer-overlays-in-range ()
  (emacs-buffer-test--with-fresh-world
    (let* ((b (nelisp-ec-generate-new-buffer "x"))
           (o1 (emacs-buffer-make-overlay 1 3 b))
           (o2 (emacs-buffer-make-overlay 5 9 b))
           (o3 (emacs-buffer-make-overlay 8 12 b)))
      ;; [4, 10) overlaps o2 + o3 but not o1.
      (let ((hits (emacs-buffer-overlays-in 4 10 b)))
        (should (= 2 (length hits)))
        (should (memq o2 hits))
        (should (memq o3 hits))
        (should-not (memq o1 hits))))))

(ert-deftest emacs-buffer-move-overlay-within-buffer ()
  (emacs-buffer-test--with-fresh-world
    (let* ((b (nelisp-ec-generate-new-buffer "x"))
           (ov (emacs-buffer-make-overlay 3 7 b)))
      (emacs-buffer-move-overlay ov 10 15)
      (should (= 10 (emacs-buffer-overlay-start ov)))
      (should (= 15 (emacs-buffer-overlay-end ov)))
      (should (eq b (emacs-buffer-overlay-buffer ov)))
      ;; overlays-at @ 5 no longer hits.
      (should-not (emacs-buffer-overlays-at 5 b))
      (should (equal (list ov) (emacs-buffer-overlays-at 12 b))))))

(ert-deftest emacs-buffer-move-overlay-across-buffers ()
  (emacs-buffer-test--with-fresh-world
    (let* ((a (nelisp-ec-generate-new-buffer "a"))
           (b (nelisp-ec-generate-new-buffer "b"))
           (ov (emacs-buffer-make-overlay 1 5 a)))
      (emacs-buffer-move-overlay ov 2 6 b)
      (should (eq b (emacs-buffer-overlay-buffer ov)))
      ;; The overlay is no longer in A's registry.
      (should-not (emacs-buffer-overlays-at 3 a))
      (should (equal (list ov) (emacs-buffer-overlays-at 3 b))))))

(ert-deftest emacs-buffer-delete-overlay-clears-buffer-ref ()
  (emacs-buffer-test--with-fresh-world
    (let* ((b (nelisp-ec-generate-new-buffer "x"))
           (ov (emacs-buffer-make-overlay 3 7 b)))
      (should-not (emacs-buffer-delete-overlay ov))
      (should-not (emacs-buffer-overlay-start ov))
      (should-not (emacs-buffer-overlay-end ov))
      (should-not (emacs-buffer-overlay-buffer ov))
      (should-not (emacs-buffer-overlays-at 5 b))
      ;; Idempotent.
      (should-not (emacs-buffer-delete-overlay ov)))))

(ert-deftest emacs-buffer-delete-all-overlays-empties-registry ()
  (emacs-buffer-test--with-fresh-world
    (let* ((b (nelisp-ec-generate-new-buffer "x")))
      (emacs-buffer-make-overlay 1 3 b)
      (emacs-buffer-make-overlay 5 7 b)
      (emacs-buffer-make-overlay 9 11 b)
      (should-not (emacs-buffer-delete-all-overlays b))
      (should-not (emacs-buffer-overlays-in 1 20 b))
      (should-not (emacs-buffer-overlays-at 2 b)))))

(ert-deftest emacs-buffer-copy-overlay-is-independent ()
  (emacs-buffer-test--with-fresh-world
    (let* ((b (nelisp-ec-generate-new-buffer "x"))
           (ov (emacs-buffer-make-overlay 3 7 b)))
      (emacs-buffer-overlay-put ov 'face 'red)
      (let ((cp (emacs-buffer-copy-overlay ov)))
        (should (emacs-buffer-overlayp cp))
        (should (not (eq cp ov)))
        (should (= 3 (emacs-buffer-overlay-start cp)))
        (should (eq 'red (emacs-buffer-overlay-get cp 'face)))
        ;; Mutating the copy must not affect OV.
        (emacs-buffer-overlay-put cp 'face 'blue)
        (should (eq 'red (emacs-buffer-overlay-get ov 'face)))
        (should (eq 'blue (emacs-buffer-overlay-get cp 'face)))
        ;; Both live in BUF's registry.
        (let ((hits (emacs-buffer-overlays-at 5 b)))
          (should (memq ov hits))
          (should (memq cp hits)))))))

(ert-deftest emacs-buffer-overlay-lists-partitions-by-point ()
  (emacs-buffer-test--with-fresh-world
    (let* ((b (nelisp-ec-generate-new-buffer "x"))
           (o1 (emacs-buffer-make-overlay 1 3 b))
           (o2 (emacs-buffer-make-overlay 5 9 b))
           (_o3 (emacs-buffer-make-overlay 10 12 b)))
      (ignore _o3)
      ;; point defaults to 1 on a fresh buffer.
      (setf (nelisp-ec-buffer-point b) 4)
      (let ((pair (emacs-buffer-overlay-lists b)))
        (should (memq o1 (car pair)))
        (should (memq o2 (cdr pair)))))))

(ert-deftest emacs-buffer-overlay-dead-ops-signal ()
  (emacs-buffer-test--with-fresh-world
    (let* ((b (nelisp-ec-generate-new-buffer "x"))
           (ov (emacs-buffer-make-overlay 3 7 b)))
      (emacs-buffer-delete-overlay ov)
      (should-error (emacs-buffer-overlay-put ov 'face 'red)
                    :type 'emacs-buffer-error)
      (should-error (emacs-buffer-overlay-get ov 'face)
                    :type 'emacs-buffer-error)
      (should-error (emacs-buffer-overlay-properties ov)
                    :type 'emacs-buffer-error)
      (should-error (emacs-buffer-move-overlay ov 1 2 b)
                    :type 'emacs-buffer-error)
      (should-error (emacs-buffer-copy-overlay ov)
                    :type 'emacs-buffer-error))))

(ert-deftest emacs-buffer-overlay-front-rear-advance-stored ()
  (emacs-buffer-test--with-fresh-world
    (let* ((b (nelisp-ec-generate-new-buffer "x"))
           (ov (emacs-buffer-make-overlay 3 7 b t t))
           (cp (emacs-buffer-copy-overlay ov)))
      ;; Phase 1 stores the flags; insertion-shift semantics arrive
      ;; with the Doc 41 §2.6 endpoint-aware insert/delete propagation.
      (should (emacs-buffer--overlay-rec-front-advance ov))
      (should (emacs-buffer--overlay-rec-rear-advance ov))
      ;; Copy preserves the flags.
      (should (emacs-buffer--overlay-rec-front-advance cp))
      (should (emacs-buffer--overlay-rec-rear-advance cp)))))

(ert-deftest emacs-buffer-overlay-tie-break-by-insertion-id ()
  (emacs-buffer-test--with-fresh-world
    (let* ((b (nelisp-ec-generate-new-buffer "x"))
           (o1 (emacs-buffer-make-overlay 5 9 b))
           (o2 (emacs-buffer-make-overlay 5 9 b))
           (o3 (emacs-buffer-make-overlay 5 9 b)))
      ;; All three start at 5; order at position 5 must be insertion order.
      (should (equal (list o1 o2 o3) (emacs-buffer-overlays-at 5 b))))))

(provide 'emacs-buffer-test)
;;; emacs-buffer-test.el ends here
