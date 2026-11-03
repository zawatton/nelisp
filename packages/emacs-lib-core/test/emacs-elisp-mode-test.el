;;; emacs-elisp-mode-test.el --- ERT for the elisp-mode font-lock pipeline -*- lexical-binding: t; -*-

;;; Commentary:

;; Doc 51 Track T (2026-05-04) — end-to-end check of the elisp-mode
;; font-lock keywords against the Tracks D / G / J / R / S
;; pipeline.  Each test loads a small elisp source into a fresh
;; substrate buffer, runs the keyword set, and asserts on the face
;; at specific positions (= what a real user would see).

;;; Code:

(require 'ert)
(require 'emacs-buffer)
(require 'emacs-font-lock)
(require 'emacs-syntax-table)
(require 'emacs-elisp-mode)

(defmacro emacs-elisp-mode-test--with-buffer (text &rest body)
  "Create a fresh substrate buffer with TEXT, run the elisp-mode
font-lock setup, then evaluate BODY with `b' bound to the buffer."
  (declare (indent 1) (debug (form body)))
  `(let ((b (nelisp-ec-generate-new-buffer "*elisp-mode-test*")))
     (unwind-protect
         (let ((nelisp-ec--current-buffer b))
           (nelisp-ec-insert ,text)
           (emacs-elisp-mode-setup-font-lock b)
           (emacs-font-lock-fontify-region 1 (1+ (length ,text)))
           ,@body)
       (when (fboundp 'nelisp-ec-kill-buffer)
         (nelisp-ec-kill-buffer b)))))

(defun emacs-elisp-mode-test--face-at (pos buf)
  "Convenience accessor."
  (emacs-buffer-get-text-property pos 'face buf))

(ert-deftest emacs-elisp-mode-test/font-lock-keywords-available ()
  (should (consp emacs-elisp-mode-font-lock-keywords)))

;;;; --- defun -----------------------------------------------------------------

(ert-deftest emacs-elisp-mode-test/defun-keyword ()
  (emacs-elisp-mode-test--with-buffer "(defun foo () 1)"
    ;; Position 2 = `defun', should be keyword-face.
    (should (eq 'font-lock-keyword-face
                (emacs-elisp-mode-test--face-at 2 b)))))

(ert-deftest emacs-elisp-mode-test/defun-function-name ()
  (emacs-elisp-mode-test--with-buffer "(defun foo () 1)"
    ;; Position 8 = `foo', should be function-name-face.
    (should (eq 'font-lock-function-name-face
                (emacs-elisp-mode-test--face-at 8 b)))))

;;;; --- defvar ----------------------------------------------------------------

(ert-deftest emacs-elisp-mode-test/defvar-keyword-and-name ()
  (emacs-elisp-mode-test--with-buffer "(defvar bar 42)"
    (should (eq 'font-lock-keyword-face
                (emacs-elisp-mode-test--face-at 2 b)))
    (should (eq 'font-lock-variable-name-face
                (emacs-elisp-mode-test--face-at 9 b)))))

;;;; --- special forms ---------------------------------------------------------

(ert-deftest emacs-elisp-mode-test/let-keyword ()
  (emacs-elisp-mode-test--with-buffer "(let ((x 1)) x)"
    (should (eq 'font-lock-keyword-face
                (emacs-elisp-mode-test--face-at 2 b)))))

(ert-deftest emacs-elisp-mode-test/lambda-keyword ()
  (emacs-elisp-mode-test--with-buffer "(lambda (n) n)"
    (should (eq 'font-lock-keyword-face
                (emacs-elisp-mode-test--face-at 2 b)))))

;;;; --- constants -------------------------------------------------------------

(ert-deftest emacs-elisp-mode-test/t-constant ()
  (emacs-elisp-mode-test--with-buffer "(if t 1 2)"
    ;; Position 5 = `t'.
    (should (eq 'font-lock-constant-face
                (emacs-elisp-mode-test--face-at 5 b)))))

(ert-deftest emacs-elisp-mode-test/keyword-constant ()
  (emacs-elisp-mode-test--with-buffer "(plist :foo 1)"
    ;; Position 8 = ":foo".
    (should (eq 'font-lock-constant-face
                (emacs-elisp-mode-test--face-at 8 b)))))

;;;; --- syntactic faces (= Track R integration) ------------------------------

(ert-deftest emacs-elisp-mode-test/string-face-overrides-keywords ()
  "A string literal that contains a keyword name (= `defun' as
text) must come out as string-face.  The syntactic post-pass
runs *after* keyword fontification so it always wins inside
strings."
  (emacs-elisp-mode-test--with-buffer "(progn \"defun-inside\" 1)"
    ;; Position 9 = inside the string.
    (should (eq 'font-lock-string-face
                (emacs-elisp-mode-test--face-at 9 b)))))

(ert-deftest emacs-elisp-mode-test/comment-face ()
  "A line comment starting with `;' uses comment-face."
  (emacs-elisp-mode-test--with-buffer "1 ; my comment"
    ;; Position 4 = the `;'.
    (should (eq 'font-lock-comment-face
                (emacs-elisp-mode-test--face-at 4 b)))
    ;; Position 8 = inside the comment text.
    (should (eq 'font-lock-comment-face
                (emacs-elisp-mode-test--face-at 8 b)))))

(ert-deftest emacs-elisp-mode-test/comment-does-not-bleed-past-newline ()
  "Comment face must terminate at the next newline."
  (emacs-elisp-mode-test--with-buffer "1 ; cmt\nmore"
    (should (eq 'font-lock-comment-face
                (emacs-elisp-mode-test--face-at 4 b)))
    ;; Position 9 = `m' on the next line, should NOT be comment-face.
    (should-not (eq 'font-lock-comment-face
                    (emacs-elisp-mode-test--face-at 9 b)))))

;;;; --- mixed source ----------------------------------------------------------

(ert-deftest emacs-elisp-mode-test/mixed-source-end-to-end ()
  "Realistic source: defun with body containing strings, comments,
constants, and a keyword.  Asserts the canonical face at each
key position."
  (emacs-elisp-mode-test--with-buffer
      "(defun greet (name)\n  ;; greet by name\n  (when name\n    \"hello\"))"
    ;; `defun' = keyword
    (should (eq 'font-lock-keyword-face
                (emacs-elisp-mode-test--face-at 2 b)))
    ;; `greet' = function-name  (= position 8)
    (should (eq 'font-lock-function-name-face
                (emacs-elisp-mode-test--face-at 8 b)))
    ;; ` ;; greet by name' starts at position 23 (= the ;)
    ;; — comment-face
    (should (eq 'font-lock-comment-face
                (emacs-elisp-mode-test--face-at 23 b)))))

(provide 'emacs-elisp-mode-test)

;;; emacs-elisp-mode-test.el ends here
