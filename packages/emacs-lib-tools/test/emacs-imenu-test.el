;;; emacs-imenu-test.el --- ERT for emacs-imenu  -*- lexical-binding: t; -*-

;;; Commentary:

;; Elisp symbol-index tests.  The scan is a pure buffer unit; goto moves point
;; in a throwaway buffer.  Validates the Layer 2 logic independently of the
;; reader.

;;; Code:

(require 'ert)
(require 'emacs-imenu)

(defconst emacs-imenu-test--source
  (concat
   "(defun foo (x) (+ x 1))\n"
   "(defvar bar 42)\n"
   "(defmacro baz (a) (list 'quote a))\n"
   "(cl-defstruct point x y)\n"
   "  (defun indented-fn () nil)\n"
   "(defalias 'aliased 'foo)\n"
   "(define-derived-mode my-mode prog-mode \"My\")\n"
   "not-a-def here\n"
   "(define-key some-map \"k\" 'cmd)\n"   ; must NOT be indexed
   "(defun foo (y) y)\n")                 ; duplicate name
  "A buffer of mixed top-level forms exercising the scanner.")

(defmacro emacs-imenu-test--with-source (&rest body)
  "Run BODY in a temp buffer filled with `emacs-imenu-test--source'."
  `(with-temp-buffer
     (insert emacs-imenu-test--source)
     ,@body))

;;;; --- scan / index -------------------------------------------------

(ert-deftest emacs-imenu-test/index-finds-all-defs-in-order ()
  (emacs-imenu-test--with-source
   (let ((index (emacs-imenu-create-index)))
     ;; 8 definitions; (define-key ...) and the non-def line are skipped.
     (should (= 8 (length index)))
     (should (equal '("foo" "bar" "baz" "point" "indented-fn"
                      "aliased" "my-mode" "foo")
                    (mapcar #'car index))))))

(ert-deftest emacs-imenu-test/define-key-not-indexed ()
  (emacs-imenu-test--with-source
   (should-not (assoc "some-map" (emacs-imenu-create-index)))))

(ert-deftest emacs-imenu-test/names-deduplicate-first-wins ()
  (emacs-imenu-test--with-source
   (should (equal '("foo" "bar" "baz" "point" "indented-fn"
                    "aliased" "my-mode")
                  (emacs-imenu--names (emacs-imenu-create-index))))))

(ert-deftest emacs-imenu-test/position-points-at-open-paren ()
  (emacs-imenu-test--with-source
   (let* ((index (emacs-imenu-create-index))
          (pos (cdr (assoc "bar" index))))
     (should (= ?\( (char-after pos)))
     ;; the `bar' form is the second line; its paren opens `(defvar bar'
     (should (string-prefix-p "(defvar bar"
                              (buffer-substring-no-properties
                               pos (min (point-max) (+ pos 11))))))))

;;;; --- goto ---------------------------------------------------------

(ert-deftest emacs-imenu-test/goto-moves-point ()
  (emacs-imenu-test--with-source
   (goto-char (point-max))
   (let ((pos (emacs-imenu-goto "baz")))
     (should (= (point) pos))
     (should (= ?\( (char-after (point))))
     (should (string-prefix-p "(defmacro baz"
                              (buffer-substring-no-properties
                               (point) (min (point-max) (+ (point) 13))))))))

(ert-deftest emacs-imenu-test/goto-unknown-signals ()
  (emacs-imenu-test--with-source
   (should-error (emacs-imenu-goto "no-such-symbol"))))

(ert-deftest emacs-imenu-test/empty-buffer-has-no-index ()
  (with-temp-buffer
    (insert ";; just a comment, no defs\n")
    (should-not (emacs-imenu-create-index))))

;;;; --- interactive entry (name supplied) ----------------------------

(ert-deftest emacs-imenu-test/imenu-with-name-jumps ()
  (emacs-imenu-test--with-source
   (goto-char (point-min))
   (let ((pos (emacs-imenu "my-mode")))
     (should (= (point) pos))
     (should (string-prefix-p "(define-derived-mode my-mode"
                              (buffer-substring-no-properties
                               (point) (min (point-max) (+ (point) 28))))))))

(provide 'emacs-imenu-test)

;;; emacs-imenu-test.el ends here
