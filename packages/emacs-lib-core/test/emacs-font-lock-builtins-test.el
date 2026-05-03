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
                 font-lock-remove-keywords font-lock-set-defaults))
    (should (fboundp sym))))

(ert-deftest emacs-font-lock-builtins-test/standard-faces-registered ()
  (dolist (face '(font-lock-keyword-face font-lock-string-face
                  font-lock-comment-face font-lock-function-name-face
                  font-lock-type-face font-lock-variable-name-face
                  font-lock-constant-face font-lock-builtin-face
                  font-lock-warning-face font-lock-doc-face))
    (should (emacs-faces-facep face))))

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

(provide 'emacs-font-lock-builtins-test)

;;; emacs-font-lock-builtins-test.el ends here
