;;; emacs-font-lock-builtins-test.el --- ERT for font-lock MVP  -*- lexical-binding: t; -*-

;;; Commentary:

;; Track K ERT.  Verifies the prefixed substrate
;; (`emacs-font-lock-*') drives keyword-form fontification through
;; the existing text-property store in `emacs-buffer.el', and that
;; the unprefixed bridges (`font-lock-mode' etc) defalias-route to
;; them.
;;
;; Behavioural assertions exercise the prefixed
;; `emacs-font-lock-*' API directly so they bypass host Emacs's
;; `font-lock.el' (= same pattern as every other Track in this
;; repo).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'emacs-buffer)
(require 'emacs-font-lock)
(require 'emacs-font-lock-builtins)

;;;; --- fixtures ------------------------------------------------------

(defmacro emacs-font-lock-builtins-test--with-buffer (name text &rest body)
  "Run BODY with a fresh nelisp-ec buffer NAME containing TEXT.
The buffer is set as current via `nelisp-ec--current-buffer'."
  (declare (indent 2) (debug (sexp form body)))
  `(let* ((nelisp-ec--buffers nil)
          (nelisp-ec--current-buffer nil)
          (b (nelisp-ec-generate-new-buffer ,name)))
     (let ((nelisp-ec--current-buffer b))
       (nelisp-ec-insert ,text)
       (nelisp-ec-goto-char 1)
       ,@body)))

;;;; A. Load + parity

(ert-deftest emacs-font-lock-builtins-test/require-loads-cleanly ()
  (should (featurep 'emacs-font-lock-builtins))
  (dolist (sym '(font-lock-mode font-lock-fontify-region
                 font-lock-fontify-buffer font-lock-add-keywords
                 font-lock-remove-keywords font-lock-set-defaults
                 font-lock-unfontify-region font-lock-unfontify-buffer
                 font-lock-default-fontify-region font-lock-specified-p
                 jit-lock-register jit-lock-unregister))
    (should (fboundp sym))))

(ert-deftest emacs-font-lock-builtins-test/require-is-idempotent ()
  (let ((before-mode (symbol-function 'font-lock-mode))
        (before-fontify (symbol-function 'font-lock-fontify-region))
        (before-add (symbol-function 'font-lock-add-keywords)))
    (require 'emacs-font-lock-builtins)
    (should (eq before-mode (symbol-function 'font-lock-mode)))
    (should (eq before-fontify (symbol-function 'font-lock-fontify-region)))
    (should (eq before-add (symbol-function 'font-lock-add-keywords)))))

(ert-deftest emacs-font-lock-builtins-test/bridge-overwrites-standalone-stubs-in-source ()
  (should (fboundp 'emacs-font-lock-builtins--install-function-p))
  (should-not (emacs-font-lock-builtins--install-function-p 'font-lock-mode))
  (let ((original-marker (get 'font-lock-mode 'emacs-stub-bulk)))
    (unwind-protect
        (progn
          (put 'font-lock-mode 'emacs-stub-bulk t)
          (should (emacs-font-lock-builtins--install-function-p
                   'font-lock-mode)))
      (put 'font-lock-mode 'emacs-stub-bulk original-marker)))
  (let* ((file (locate-library "emacs-font-lock-builtins"))
         (file (if (and file (string-match-p "\\.elc\\'" file))
                   (concat (substring file 0 (- (length file) 1)))
                 file)))
    (should (and file (file-exists-p file)))
    (with-temp-buffer
      (insert-file-contents file)
      (dolist (sym '(font-lock-mode font-lock-fontify-region
                     font-lock-fontify-buffer font-lock-unfontify-region
                     font-lock-unfontify-buffer font-lock-default-fontify-region
                     font-lock-add-keywords font-lock-remove-keywords
                     font-lock-set-defaults font-lock-specified-p jit-lock-register
                     jit-lock-unregister))
        (goto-char (point-min))
        (should (search-forward
                 (format "(when (emacs-font-lock-builtins--install-function-p '%s)"
                         sym)
                 nil t))))))

(ert-deftest emacs-font-lock-builtins-test/standard-faces-registered ()
  (dolist (face '(font-lock-keyword-face font-lock-string-face
                  font-lock-comment-face font-lock-function-name-face
                  font-lock-type-face font-lock-variable-name-face
                  font-lock-constant-face font-lock-builtin-face
                  font-lock-warning-face font-lock-doc-face))
    (should (emacs-faces-facep face))))

(ert-deftest emacs-font-lock-builtins-test/vendor-font-core-state-vars-bound ()
  "Vendor `font-core.el' expects these core variables before toggling."
  (dolist (sym '(font-lock-major-mode char-property-alias-alist))
    (should (boundp sym))))

(ert-deftest emacs-font-lock-builtins-test/font-lock-specified-p-subset ()
  "The lightweight `font-lock-specified-p' follows vendor's nil-default case."
  (let ((font-lock-defaults nil)
        (font-lock-keywords nil)
        (font-lock-set-defaults nil)
        (font-lock-major-mode nil)
        (major-mode 'fundamental-mode))
    (should-not (font-lock-specified-p nil))
    (let ((font-lock-defaults '(("x"))))
      (should (font-lock-specified-p nil)))
    (let ((font-lock-keywords '(("x"))))
      (should (font-lock-specified-p nil)))))

(ert-deftest emacs-font-lock-builtins-test/default-fontify-region-applies-syntax-faces ()
  (emacs-font-lock-builtins-test--with-buffer "fld-default" "(progn \"hello\" ; note)\n"
    (let ((font-lock-keywords '(("progn" (0 font-lock-keyword-face)))))
      (should (emacs-font-lock-default-fontify-region 1 23 nil b))
      (should (eq 'font-lock-keyword-face
                  (emacs-buffer-get-text-property 2 'face b)))
      (should (eq 'font-lock-string-face
                  (emacs-buffer-get-text-property 9 'face b)))
      (should (eq 'font-lock-comment-face
                  (emacs-buffer-get-text-property 17 'face b))))))

;;;; B. keyword compilation

(ert-deftest emacs-font-lock-builtins-test/compile-string-keyword ()
  (let ((compiled (emacs-font-lock--compile-keyword "foo")))
    (should (equal (car compiled) "foo"))
    (should (equal (cadr compiled) '(0 font-lock-keyword-face nil nil)))))

(ert-deftest emacs-font-lock-builtins-test/compile-cons-face ()
  (let ((compiled (emacs-font-lock--compile-keyword
                   '("foo" . font-lock-string-face))))
    (should (equal (car compiled) "foo"))
    (should (equal (cadr compiled) '(0 font-lock-string-face nil nil)))))

(ert-deftest emacs-font-lock-builtins-test/compile-list-highlights ()
  (let ((compiled (emacs-font-lock--compile-keyword
                   '("foo\\(bar\\)" (1 font-lock-type-face)))))
    (should (equal (car compiled) "foo\\(bar\\)"))
    (should (equal (cadr compiled) '(1 font-lock-type-face nil nil)))))

;;;; C. set-defaults

(ert-deftest emacs-font-lock-builtins-test/set-defaults-from-list ()
  (emacs-font-lock-builtins-test--with-buffer "fld-defs" "x"
    (let ((font-lock-defaults '(("foo" "bar"))))
      (emacs-font-lock-set-defaults b)
      (let ((kws (emacs-font-lock-keywords b)))
        (should (= 2 (length kws)))
        (should (equal "foo" (car (nth 0 kws))))
        (should (equal "bar" (car (nth 1 kws))))))))

(ert-deftest emacs-font-lock-builtins-test/set-defaults-nil-leaves-empty ()
  (emacs-font-lock-builtins-test--with-buffer "fld-defs-nil" "x"
    (let ((font-lock-defaults nil))
      (emacs-font-lock-set-defaults b)
      (should (null (emacs-font-lock-keywords b))))))

;;;; D. add / remove keywords

(ert-deftest emacs-font-lock-builtins-test/add-keywords-prepend ()
  (emacs-font-lock-builtins-test--with-buffer "fld-add" "x"
    (emacs-font-lock-add-keywords nil '("first") nil)
    (emacs-font-lock-add-keywords nil '("second") nil)
    (let ((kws (emacs-font-lock-keywords b)))
      (should (equal "second" (car (nth 0 kws))))
      (should (equal "first"  (car (nth 1 kws)))))))

(ert-deftest emacs-font-lock-builtins-test/add-keywords-append ()
  (emacs-font-lock-builtins-test--with-buffer "fld-add-app" "x"
    (emacs-font-lock-add-keywords nil '("first") nil)
    (emacs-font-lock-add-keywords nil '("second") 'append)
    (let ((kws (emacs-font-lock-keywords b)))
      (should (equal "first"  (car (nth 0 kws))))
      (should (equal "second" (car (nth 1 kws)))))))

(ert-deftest emacs-font-lock-builtins-test/add-keywords-set-replaces ()
  (emacs-font-lock-builtins-test--with-buffer "fld-add-set" "x"
    (emacs-font-lock-add-keywords nil '("first") nil)
    (emacs-font-lock-add-keywords nil '("second") 'set)
    (let ((kws (emacs-font-lock-keywords b)))
      (should (= 1 (length kws)))
      (should (equal "second" (car (nth 0 kws)))))))

(ert-deftest emacs-font-lock-builtins-test/remove-keywords ()
  (emacs-font-lock-builtins-test--with-buffer "fld-rm" "x"
    (emacs-font-lock-add-keywords nil '("foo" "bar" "baz") 'set)
    (emacs-font-lock-remove-keywords nil '("bar"))
    (let ((kws (emacs-font-lock-keywords b)))
      (should (= 2 (length kws)))
      (should (equal '("foo" "baz") (mapcar #'car kws))))))

;;;; E. fontify-region applies face text-property

(ert-deftest emacs-font-lock-builtins-test/fontify-region-string-keyword ()
  (emacs-font-lock-builtins-test--with-buffer "fld-fr" "hello world"
    (emacs-font-lock-add-keywords nil '("hello") 'set)
    (emacs-font-lock-fontify-region 1 12)
    ;; "hello" occupies positions 1..6 (1-based, end exclusive).
    (should (eq 'font-lock-keyword-face
                (emacs-buffer-get-text-property 1 'face b)))
    (should (eq 'font-lock-keyword-face
                (emacs-buffer-get-text-property 5 'face b)))
    (should (null (emacs-buffer-get-text-property 7 'face b)))))

(ert-deftest emacs-font-lock-builtins-test/fontify-region-cons-face ()
  (emacs-font-lock-builtins-test--with-buffer "fld-fr-cons" "abc def"
    (emacs-font-lock-add-keywords nil
                                  '(("def" . font-lock-string-face))
                                  'set)
    (emacs-font-lock-fontify-region 1 8)
    (should (eq 'font-lock-string-face
                (emacs-buffer-get-text-property 5 'face b)))
    (should (null (emacs-buffer-get-text-property 1 'face b)))))

(ert-deftest emacs-font-lock-builtins-test/fontify-region-subexp-highlight ()
  (emacs-font-lock-builtins-test--with-buffer "fld-fr-sub" "abc-XYZ-end"
    (emacs-font-lock-add-keywords
     nil '(("abc-\\([A-Z]+\\)-end" (1 font-lock-type-face)))
     'set)
    (emacs-font-lock-fontify-region 1 12)
    ;; Whole match starts at 1, subexp 1 ("XYZ") at position 5..8.
    (should (eq 'font-lock-type-face
                (emacs-buffer-get-text-property 5 'face b)))
    (should (null (emacs-buffer-get-text-property 1 'face b)))))

(ert-deftest emacs-font-lock-builtins-test/fontify-region-multi-highlight ()
  (emacs-font-lock-builtins-test--with-buffer
      "fld-fr-multi" "alpha 42"
    (emacs-font-lock-add-keywords
     nil '(("\\(alpha\\) \\([0-9]+\\)"
            (1 font-lock-keyword-face)
            (2 font-lock-constant-face)))
     'set)
    (emacs-font-lock-fontify-region 1 9)
    (should (eq 'font-lock-keyword-face
                (emacs-buffer-get-text-property 1 'face b)))
    (should (eq 'font-lock-constant-face
                (emacs-buffer-get-text-property 7 'face b)))))

;;;; F. fontify-buffer covers full extent

(ert-deftest emacs-font-lock-builtins-test/fontify-buffer-full-range ()
  (emacs-font-lock-builtins-test--with-buffer
      "fld-fb" "foo bar foo"
    (emacs-font-lock-add-keywords nil '("foo") 'set)
    (emacs-font-lock-fontify-buffer)
    ;; First "foo" at 1..4, second at 9..12.
    (should (eq 'font-lock-keyword-face
                (emacs-buffer-get-text-property 1 'face b)))
    (should (eq 'font-lock-keyword-face
                (emacs-buffer-get-text-property 9 'face b)))
    (should (null (emacs-buffer-get-text-property 5 'face b)))))

;;;; G. unfontify-region clears face

(ert-deftest emacs-font-lock-builtins-test/unfontify-region-clears-face ()
  (emacs-font-lock-builtins-test--with-buffer
      "fld-uf" "hello"
    (emacs-font-lock-add-keywords nil '("hello") 'set)
    (emacs-font-lock-fontify-buffer)
    (should (eq 'font-lock-keyword-face
                (emacs-buffer-get-text-property 1 'face b)))
    (emacs-font-lock-unfontify-region 1 6 b)
    (should (null (emacs-buffer-get-text-property 1 'face b)))))

(ert-deftest emacs-font-lock-builtins-test/unfontify-buffer-clears-all ()
  (emacs-font-lock-builtins-test--with-buffer
      "fld-ufb" "foo bar"
    (emacs-font-lock-add-keywords nil '("foo" "bar") 'set)
    (emacs-font-lock-fontify-buffer)
    (emacs-font-lock-unfontify-buffer b)
    (should (null (emacs-buffer-get-text-property 1 'face b)))
    (should (null (emacs-buffer-get-text-property 5 'face b)))))

;;;; H. mode toggle drives fontification

(ert-deftest emacs-font-lock-builtins-test/mode-on-fontifies ()
  (emacs-font-lock-builtins-test--with-buffer
      "fld-mode-on" "hello"
    (let ((font-lock-defaults '(("hello"))))
      (should (null (emacs-font-lock-mode-enabled-p b)))
      (let ((res (emacs-font-lock-mode 1)))
        (should res))
      (should (emacs-font-lock-mode-enabled-p b))
      (should (eq 'font-lock-keyword-face
                  (emacs-buffer-get-text-property 1 'face b))))))

(ert-deftest emacs-font-lock-builtins-test/mode-off-unfontifies ()
  (emacs-font-lock-builtins-test--with-buffer
      "fld-mode-off" "hello"
    (let ((font-lock-defaults '(("hello"))))
      (emacs-font-lock-mode 1)
      (should (eq 'font-lock-keyword-face
                  (emacs-buffer-get-text-property 1 'face b)))
      (emacs-font-lock-mode 0)
      (should-not (emacs-font-lock-mode-enabled-p b))
      (should (null (emacs-buffer-get-text-property 1 'face b))))))

(ert-deftest emacs-font-lock-builtins-test/mode-toggle ()
  (emacs-font-lock-builtins-test--with-buffer
      "fld-mode-toggle" "hello"
    (let ((font-lock-defaults '(("hello"))))
      (should-not (emacs-font-lock-mode-enabled-p b))
      (emacs-font-lock-mode)
      (should (emacs-font-lock-mode-enabled-p b))
      (emacs-font-lock-mode)
      (should-not (emacs-font-lock-mode-enabled-p b)))))

;;;; I. zero-width regexp does not loop forever

(ert-deftest emacs-font-lock-builtins-test/zero-width-regexp-terminates ()
  (emacs-font-lock-builtins-test--with-buffer
      "fld-zero" "abc"
    (emacs-font-lock-add-keywords nil '("a*") 'set)
    ;; If the regexp engine matches the empty string at every position,
    ;; the loop must still terminate.  Wrap in a 1-second timer guard
    ;; via `with-timeout' if available; otherwise rely on the test
    ;; harness ERT default timeout.
    (emacs-font-lock-fontify-region 1 4)
    (should t)))

;;;; J. Track D hardening (Doc 51, 2026-05-04)

(ert-deftest emacs-font-lock-builtins-test/compile-eval-keyword-form ()
  "An (eval . FORM) keyword should round-trip through `compile-keyword'
as the sentinel `(:eval . FORM)' so it can be evaluated lazily."
  (let* ((kw '(eval . (list "abc" 0 'font-lock-string-face)))
         (compiled (emacs-font-lock--compile-keyword kw)))
    (should (eq :eval (car compiled)))
    (should (equal '(list "abc" 0 'font-lock-string-face)
                   (cdr compiled)))))

(ert-deftest emacs-font-lock-builtins-test/eval-keyword-fontifies ()
  "An (eval . FORM) keyword should fontify after the FORM is materialised."
  (emacs-font-lock-builtins-test--with-buffer
      "fld-eval-kw" "abc xyz abc"
    (emacs-font-lock-add-keywords
     nil '((eval . (cons "abc" 'font-lock-string-face)))
     'set)
    (emacs-font-lock-fontify-region 1 12)
    (should (eq 'font-lock-string-face
                (emacs-buffer-get-text-property 1 'face b)))
    (should (eq 'font-lock-string-face
                (emacs-buffer-get-text-property 9 'face b)))))

(ert-deftest emacs-font-lock-builtins-test/case-fold-honoured ()
  "When `font-lock-defaults' slot 3 is non-nil, search is case-insensitive."
  (emacs-font-lock-builtins-test--with-buffer
      "fld-case" "Hello WORLD"
    (let ((font-lock-defaults '(("hello") nil t)))
      (emacs-font-lock-set-defaults b)
      (emacs-font-lock-fontify-region 1 12)
      (should (eq 'font-lock-keyword-face
                  (emacs-buffer-get-text-property 1 'face b))))))

(ert-deftest emacs-font-lock-builtins-test/case-sensitive-default ()
  "Without slot-3 case-fold, search is case-sensitive (= no match)."
  (emacs-font-lock-builtins-test--with-buffer
      "fld-case-sens" "Hello WORLD"
    (let ((font-lock-defaults '(("hello"))))
      (emacs-font-lock-set-defaults b)
      (emacs-font-lock-fontify-region 1 12)
      (should-not (emacs-buffer-get-text-property 1 'face b)))))

(ert-deftest emacs-font-lock-builtins-test/override-keep-skips-existing ()
  "`keep' override keeps the existing face property."
  (emacs-font-lock-builtins-test--with-buffer
      "fld-keep" "abc"
    (emacs-buffer-put-text-property 1 4 'face 'font-lock-comment-face b)
    (emacs-font-lock-add-keywords
     nil '(("abc" (0 font-lock-keyword-face keep)))
     'set)
    (emacs-font-lock-fontify-region 1 4)
    (should (eq 'font-lock-comment-face
                (emacs-buffer-get-text-property 1 'face b)))))

(ert-deftest emacs-font-lock-builtins-test/override-prepend-merges ()
  "`prepend' override builds a face list with NEW at the head."
  (emacs-font-lock-builtins-test--with-buffer
      "fld-prepend" "abc"
    (emacs-buffer-put-text-property 1 4 'face 'font-lock-comment-face b)
    (emacs-font-lock-add-keywords
     nil '(("abc" (0 font-lock-keyword-face prepend)))
     'set)
    (emacs-font-lock-fontify-region 1 4)
    (let ((cur (emacs-buffer-get-text-property 1 'face b)))
      (should (listp cur))
      (should (memq 'font-lock-keyword-face cur))
      (should (memq 'font-lock-comment-face cur))
      (should (eq 'font-lock-keyword-face (car cur))))))

(ert-deftest emacs-font-lock-builtins-test/override-append-merges ()
  "`append' override builds a face list with NEW at the tail."
  (emacs-font-lock-builtins-test--with-buffer
      "fld-append" "abc"
    (emacs-buffer-put-text-property 1 4 'face 'font-lock-comment-face b)
    (emacs-font-lock-add-keywords
     nil '(("abc" (0 font-lock-keyword-face append)))
     'set)
    (emacs-font-lock-fontify-region 1 4)
    (let ((cur (emacs-buffer-get-text-property 1 'face b)))
      (should (listp cur))
      (should (eq 'font-lock-comment-face (car cur))))))

;;;; K. Track G — anchored matcher (Doc 51, 2026-05-04)

(ert-deftest emacs-font-lock-builtins-test/compile-anchored-matcher ()
  "Anchored entries (REGEX PRE POST INNER...) should compile to
the `(:anchored REGEX PRE POST (CANONICAL-INNERS))' sentinel."
  (let* ((kw '("foo"
               ("bar" nil nil (0 font-lock-string-face))))
         (compiled (emacs-font-lock--compile-keyword kw)))
    (should (equal "foo" (car compiled)))
    (let ((anc (cadr compiled)))
      (should (eq :anchored (car anc)))
      (should (equal "bar" (nth 1 anc)))
      (should-not (nth 2 anc))   ; PRE-FORM = nil
      (should-not (nth 3 anc))   ; POST-FORM = nil
      ;; HIGHLIGHTS canonicalised.
      (should (equal '((0 font-lock-string-face nil nil))
                     (nth 4 anc))))))

(ert-deftest emacs-font-lock-builtins-test/anchored-fontifies-after-outer ()
  "Anchored matcher should fontify text matching the inner regex
after each outer match.  Test text: `func arg1 arg2' — outer
matches `func' (= keyword), anchored matches the args (= variable).
After Doc 51 Track J, word-boundary anchors `\\<' / `\\>' are
supported and used here for realistic font-lock-keywords parity."
  (emacs-font-lock-builtins-test--with-buffer
      "fld-anchored" "func arg1 arg2"
    (emacs-font-lock-add-keywords
     nil
     '(("func"
        (0 font-lock-keyword-face)
        ("\\<\\([a-z]+[0-9]+\\)\\>" nil nil (1 font-lock-variable-name-face))))
     'set)
    (emacs-font-lock-fontify-region 1 (1+ (length "func arg1 arg2")))
    (should (eq 'font-lock-keyword-face
                (emacs-buffer-get-text-property 1 'face b)))
    (should (eq 'font-lock-variable-name-face
                (emacs-buffer-get-text-property 6 'face b)))
    (should (eq 'font-lock-variable-name-face
                (emacs-buffer-get-text-property 11 'face b)))))

(ert-deftest emacs-font-lock-builtins-test/anchored-pre-form-bounds-search ()
  "PRE-FORM evaluating to an integer bound should constrain the
inner search to that bound (= won't fontify past it)."
  (emacs-font-lock-builtins-test--with-buffer
      "fld-anchored-bound" "func a1 a2 a3"
    (emacs-font-lock-add-keywords
     nil
     ;; Limit anchored search to position 9 — only catches `a1', not later.
     '(("func"
        (0 font-lock-keyword-face)
        ("\\([a-z][0-9]\\)" 9 nil (1 font-lock-variable-name-face))))
     'set)
    (emacs-font-lock-fontify-region 1 (1+ (length "func a1 a2 a3")))
    (should (eq 'font-lock-variable-name-face
                (emacs-buffer-get-text-property 6 'face b)))
    ;; Position 9-10 = `a2'.  Bound was 9 so a2 should NOT be matched.
    (should-not (eq 'font-lock-variable-name-face
                    (emacs-buffer-get-text-property 9 'face b)))))

(ert-deftest emacs-font-lock-builtins-test/anchored-no-outer-no-inner ()
  "If the outer regex has no match, the anchored block must not run."
  (emacs-font-lock-builtins-test--with-buffer
      "fld-anchored-empty" "abc def"
    (emacs-font-lock-add-keywords
     nil
     '(("zzzz"
        (0 font-lock-keyword-face)
        ("[a-z]+" nil nil (0 font-lock-variable-name-face))))
     'set)
    (emacs-font-lock-fontify-region 1 (1+ (length "abc def")))
    ;; No outer match, so nothing should be fontified.
    (should-not (emacs-buffer-get-text-property 1 'face b))
    (should-not (emacs-buffer-get-text-property 5 'face b))))

;;;; L. Track S — jit-lock primitives (Doc 51, 2026-05-04)

(ert-deftest emacs-font-lock-builtins-test/track-s-mark-dirty-region-stores ()
  (emacs-font-lock-builtins-test--with-buffer
      "fld-mark-dirty" "abcdef"
    (emacs-font-lock-mark-dirty-region 2 5 b)
    (let ((d (emacs-font-lock-pending-dirty-region b)))
      (should (equal '(2 . 5) d)))))

(ert-deftest emacs-font-lock-builtins-test/track-s-mark-dirty-coalesces ()
  "Multiple `mark-dirty' calls union into one (min-start . max-end)
interval, NOT a list."
  (emacs-font-lock-builtins-test--with-buffer
      "fld-coalesce" "abcdef"
    (emacs-font-lock-mark-dirty-region 3 5 b)
    (emacs-font-lock-mark-dirty-region 1 4 b)
    (let ((d (emacs-font-lock-pending-dirty-region b)))
      (should (equal '(1 . 5) d)))))

(ert-deftest emacs-font-lock-builtins-test/track-s-mark-dirty-reuses-interval ()
  "Repeated dirty marks should extend the same cons cell."
  (emacs-font-lock-builtins-test--with-buffer
      "fld-coalesce-eq" "abcdef"
    (let ((first (emacs-font-lock-mark-dirty-region 3 4 b)))
      (should (eq first (emacs-font-lock-mark-dirty-region 2 5 b)))
      (should (eq first (emacs-font-lock-pending-dirty-region b)))
      (should (equal '(2 . 5) first)))))

(ert-deftest emacs-font-lock-builtins-test/track-s-flush-pending-clears-marker ()
  "After flush, the pending interval is nil regardless of whether
font-lock-mode was enabled (= the marker must not leak)."
  (emacs-font-lock-builtins-test--with-buffer
      "fld-flush" "abcdef"
    (emacs-font-lock-mark-dirty-region 2 4 b)
    (emacs-font-lock-flush-pending b)
    (should-not (emacs-font-lock-pending-dirty-region b))))

(ert-deftest emacs-font-lock-builtins-test/track-s-flush-fontifies-only-dirty ()
  "Flush re-fontifies the dirty interval only — positions outside
the interval keep whatever face they had before."
  (emacs-font-lock-builtins-test--with-buffer
      "fld-only-dirty" "foo bar"
    (emacs-font-lock-add-keywords
     nil
     '(("foo" (0 font-lock-keyword-face))
       ("bar" (0 font-lock-keyword-face)))
     'set)
    (emacs-font-lock-mode 1)
    ;; mode-on triggered an initial whole-buffer fontify; both faces present.
    (should (eq 'font-lock-keyword-face
                (emacs-buffer-get-text-property 1 'face b)))
    (should (eq 'font-lock-keyword-face
                (emacs-buffer-get-text-property 5 'face b)))
    ;; Mark only positions 5-7 dirty (= "bar"); flush.
    (emacs-buffer-put-text-property 1 4 'face nil b)
    (emacs-buffer-put-text-property 5 8 'face nil b)
    (emacs-font-lock-mark-dirty-region 5 8 b)
    (emacs-font-lock-flush-pending b)
    ;; "bar" is re-fontified; "foo" stays nil (= we cleared it and
    ;; didn't include it in the dirty interval).
    (should-not (emacs-buffer-get-text-property 1 'face b))
    (should (eq 'font-lock-keyword-face
                (emacs-buffer-get-text-property 5 'face b)))))

(ert-deftest emacs-font-lock-builtins-test/track-s-flush-no-dirty-returns-nil ()
  (emacs-font-lock-builtins-test--with-buffer
      "fld-noop" "abc"
    (should-not (emacs-font-lock-flush-pending b))))

(ert-deftest emacs-font-lock-builtins-test/track-s-after-change-handler-marks ()
  "The canonical hook-shape handler (BEG END LEN) routes through to
`emacs-font-lock-mark-dirty-region'."
  (emacs-font-lock-builtins-test--with-buffer
      "fld-hook-shape" "xxxxxxx"
    (emacs-font-lock-after-change-handler 2 6 0 b)
    (let ((d (emacs-font-lock-pending-dirty-region b)))
      (should (equal '(2 . 6) d)))))

(ert-deftest emacs-font-lock-builtins-test/track-s-jit-lock-registers ()
  (emacs-font-lock-builtins-test--with-buffer
      "fld-jit-register" "xxxxxxx"
    (let ((jit-lock-functions nil)
          (fn (lambda (_beg _end))))
      (should (eq fn (emacs-font-lock-jit-lock-register fn)))
      (should (eq fn (car (emacs-font-lock-jit-lock-functions b))))
      (should (eq fn (car jit-lock-functions)))
      (emacs-font-lock-jit-lock-register fn)
      (should (= 1 (length (emacs-font-lock-jit-lock-functions b))))
      (should (= 1 (length jit-lock-functions))))))

(ert-deftest emacs-font-lock-builtins-test/track-s-jit-lock-unregisters ()
  (emacs-font-lock-builtins-test--with-buffer
      "fld-jit-unregister" "xxxxxxx"
    (let ((jit-lock-functions nil)
          (fn (lambda (_beg _end))))
      (emacs-font-lock-jit-lock-register fn)
      (should (eq fn (emacs-font-lock-jit-lock-unregister fn)))
      (should-not (memq fn (emacs-font-lock-jit-lock-functions b)))
      (should-not (memq fn jit-lock-functions)))))

(provide 'emacs-font-lock-builtins-test)

;;; emacs-font-lock-builtins-test.el ends here
