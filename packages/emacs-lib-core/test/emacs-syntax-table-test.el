;;; emacs-syntax-table-test.el --- ERT tests for the syntax-table MVP -*- lexical-binding: t; -*-

;;; Commentary:

;; Doc 51 Track R (2026-05-04) — coverage for the syntax class lookup
;; + the font-lock pre-pass that faces strings and line comments.

;;; Code:

(require 'ert)
(require 'emacs-syntax-table)
(require 'emacs-font-lock)
(require 'emacs-buffer)

;;;; --- helpers ---------------------------------------------------------------

(defmacro emacs-syntax-table-test--with-buffer (text &rest body)
  "Create a fresh buffer with TEXT loaded, bind it as `b'.

The buffer is the prefixed substrate's `nelisp-ec-generate-new-buffer'
(matches the existing emacs-font-lock-builtins-test pattern)."
  (declare (indent 1) (debug (form body)))
  `(let ((b (nelisp-ec-generate-new-buffer "*syntax-test*")))
     (unwind-protect
         (let ((nelisp-ec--current-buffer b))
           (nelisp-ec-insert ,text)
           ,@body)
       (when (fboundp 'nelisp-ec-kill-buffer)
         (nelisp-ec-kill-buffer b)))))

;;;; --- syntax-class lookup ---------------------------------------------------

(ert-deftest emacs-syntax-table-test/standard-classes ()
  (should (eq 'string-fence  (emacs-syntax-class-of ?\")))
  (should (eq 'escape        (emacs-syntax-class-of ?\\)))
  (should (eq 'comment-start (emacs-syntax-class-of ?\;)))
  (should (eq 'comment-end   (emacs-syntax-class-of ?\n)))
  (should (eq 'open          (emacs-syntax-class-of ?\()))
  (should (eq 'close         (emacs-syntax-class-of ?\))))
  (should (eq 'whitespace    (emacs-syntax-class-of ?\s)))
  ;; Default class for letters / digits = `word'.
  (should (eq 'word (emacs-syntax-class-of ?a)))
  (should (eq 'word (emacs-syntax-class-of ?0))))

(ert-deftest emacs-syntax-table-test/modify-entry ()
  (let ((tbl (make-hash-table :test 'eql)))
    (should (eq 'open (emacs-syntax-modify-entry ?\( 'open tbl)))
    (should (eq 'open (emacs-syntax-class-of ?\( tbl)))
    ;; nil class clears.
    (emacs-syntax-modify-entry ?\( nil tbl)
    (should (eq 'word (emacs-syntax-class-of ?\( tbl)))))

;;;; --- syntax-state-at -------------------------------------------------------

(ert-deftest emacs-syntax-table-test/state-at-start-of-buffer ()
  (emacs-syntax-table-test--with-buffer "abc"
    (should (eq 'code (emacs-syntax-state-at 1 b)))))

(ert-deftest emacs-syntax-table-test/state-at-inside-string ()
  (emacs-syntax-table-test--with-buffer "abc \"hello\" def"
    ;; Position 7 is inside "hello".
    (should (eq 'string (emacs-syntax-state-at 7 b)))))

(ert-deftest emacs-syntax-table-test/state-at-inside-comment ()
  (emacs-syntax-table-test--with-buffer "code ;; rest of line\nmore"
    ;; Position 12 is inside the comment.
    (should (eq 'comment (emacs-syntax-state-at 12 b)))))

(ert-deftest emacs-syntax-table-test/state-after-comment-newline ()
  (emacs-syntax-table-test--with-buffer "; cmt\nafter"
    ;; Position 7 (= start of "after") is back to code.
    (should (eq 'code (emacs-syntax-state-at 7 b)))))

(ert-deftest emacs-syntax-table-test/state-respects-escape-in-string ()
  (emacs-syntax-table-test--with-buffer "\"a\\\"b\" tail"
    ;; Position 5 is inside the string (= the b).  The escaped \"
    ;; must NOT close the string.
    (should (eq 'string (emacs-syntax-state-at 5 b)))
    ;; Position 8 is after the closing fence — back to code.
    (should (eq 'code (emacs-syntax-state-at 8 b)))))

;;;; --- font-lock pre-pass ----------------------------------------------------

(ert-deftest emacs-syntax-table-test/apply-faces-string-region ()
  (emacs-syntax-table-test--with-buffer "x \"hello\" y"
    (emacs-syntax-apply-faces-region 1 (1+ (length "x \"hello\" y")) b)
    ;; Position 3 = the opening `"', should be string-face.
    (should (eq 'font-lock-string-face
                (emacs-buffer-get-text-property 3 'face b)))
    ;; Position 9 = the closing `"', should be string-face.
    (should (eq 'font-lock-string-face
                (emacs-buffer-get-text-property 9 'face b)))
    ;; Position 1 = `x', should NOT have a face.
    (should-not (emacs-buffer-get-text-property 1 'face b))))

(ert-deftest emacs-syntax-table-test/apply-faces-comment-region ()
  (emacs-syntax-table-test--with-buffer "x ; cmt\ny"
    (emacs-syntax-apply-faces-region 1 (1+ (length "x ; cmt\ny")) b)
    ;; Position 3 = `;', should be comment-face.
    (should (eq 'font-lock-comment-face
                (emacs-buffer-get-text-property 3 'face b)))
    ;; Position 5 = inside the comment, should be comment-face.
    (should (eq 'font-lock-comment-face
                (emacs-buffer-get-text-property 5 'face b)))
    ;; Position 9 = `y' on the next line, should NOT.
    (should-not (emacs-buffer-get-text-property 9 'face b))))

(ert-deftest emacs-syntax-table-test/font-lock-keyword-loses-to-string ()
  "When a keyword regex would match inside a string literal, the
syntactic post-pass must overwrite the keyword face with the
string face."
  (emacs-syntax-table-test--with-buffer "before \"defun-inside\" after"
    (emacs-font-lock-add-keywords
     nil
     '(("defun-inside" (0 font-lock-keyword-face)))
     'set)
    (emacs-font-lock-fontify-region 1 (1+ (length "before \"defun-inside\" after")))
    ;; Position 9 = inside the string, was potentially keyword-face,
    ;; must be string-face after the post-pass.
    (should (eq 'font-lock-string-face
                (emacs-buffer-get-text-property 9 'face b)))))

(provide 'emacs-syntax-table-test)

;;; emacs-syntax-table-test.el ends here
