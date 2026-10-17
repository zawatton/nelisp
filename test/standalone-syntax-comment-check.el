;;; standalone-syntax-comment-check.el --- scanners skip comments -*- lexical-binding: t -*-

;; Usage: nelisp --load test/standalone-syntax-comment-check.el
;;
;; `modify-syntax-entry' dropped the comment flags of a descriptor (the
;; "124b" in ". 124b"), and scan-lists and scan-sexps never consulted
;; `parse-sexp-ignore-comments', so C's /* */ and // comments were read as
;; code: CC Mode's col-0-paren self-test in cc-defs.el got 12 from a
;; backward scan over a buffer that is one comment, where GNU says nil.
;; Each case below is what GNU Emacs 31.1 answers.  Prints one
;; SYNTAX-COMMENT line and exits non-zero on failure.

(defvar parse-sexp-ignore-comments)
(defvar syntax-comment-failures nil)

(defun syntax-comment-c (text from n ignore)
  (with-temp-buffer
    (let ((parse-sexp-ignore-comments ignore))
      (set-syntax-table (make-syntax-table))
      (modify-syntax-entry ?\' "\"")
      (modify-syntax-entry ?/ ". 124b")
      (modify-syntax-entry ?* ". 23")
      (modify-syntax-entry ?\n "> b")
      (insert text)
      (condition-case e (scan-sexps (or from (point)) n) (error (list 'error (car e)))))))

(defun syntax-comment-lisp (text from n ignore)
  (with-temp-buffer
    (let ((parse-sexp-ignore-comments ignore))
      (set-syntax-table (make-syntax-table))
      (modify-syntax-entry ?\; "<")
      (modify-syntax-entry ?\n ">")
      (insert text)
      (condition-case e
          (list (scan-sexps (or from (point)) n) (scan-lists (or from (point)) n 0))
        (error (list 'error (car e)))))))

(defun syntax-comment-property ()
  ;; Flags from a `syntax-table' property: #| ... |# as a comment.
  (with-temp-buffer
    (let ((parse-sexp-ignore-comments t) (parse-sexp-lookup-properties t))
      (set-syntax-table (make-syntax-table))
      (insert "a #| (x |# b")
      (put-text-property 3 4 'syntax-table (cons (logior 1 (ash (logior 1 8) 16)) nil))
      (put-text-property 4 5 'syntax-table (cons (logior 1 (ash (logior 2 4) 16)) nil))
      (put-text-property 9 10 'syntax-table (cons (logior 1 (ash (logior 2 4) 16)) nil))
      (put-text-property 10 11 'syntax-table (cons (logior 1 (ash (logior 1 8) 16)) nil))
      (list (scan-sexps (point-max) -2) (scan-sexps 2 1)))))

(defmacro syntax-comment-expect (name form expected)
  `(let ((got (condition-case err ,form (error (list 'error err)))))
     (unless (equal got ,expected)
       (push (list ,name got ,expected) syntax-comment-failures))))

(syntax-comment-expect 'cc-col0-self-test (syntax-comment-c "/* '\n   () */" nil -1 t) nil)
(syntax-comment-expect 'cc-col0-not-ignoring (syntax-comment-c "/* '\n   () */" nil -1 nil) 9)
(syntax-comment-expect 'c-back-over-comment (syntax-comment-c "foo /* x */" nil -1 t) 1)
(syntax-comment-expect 'c-back-paren-in-comment (syntax-comment-c "foo /* x ) */ " nil -1 t) 1)
(syntax-comment-expect 'c-back-line-comment (syntax-comment-c "a // (x\nb" nil -1 t) 9)
(syntax-comment-expect 'c-fwd-line-comment (syntax-comment-c "a // (x\nb" 2 1 t) 10)
(syntax-comment-expect 'c-fwd-block (syntax-comment-c "/* a */ b" 1 1 t) 10)
(syntax-comment-expect 'c-fwd-paren-in-comment (syntax-comment-c "/* ( */ (b) c" 1 1 t) 12)
(syntax-comment-expect 'c-fwd-list-comment (syntax-comment-c "(a /* ) */ b)" 1 1 t) 14)
(syntax-comment-expect 'c-back-list-comment (syntax-comment-c "(a /* ( */ b)" 14 -1 t) 1)
(syntax-comment-expect 'c-back-two-comments (syntax-comment-c "x /* a */ /* b */" nil -1 t) 1)
(syntax-comment-expect 'lisp-fwd-comment (syntax-comment-lisp "; c\n(x)" 1 1 t) '(8 8))
(syntax-comment-expect 'lisp-fwd-not-ignoring (syntax-comment-lisp "; c\n(x)" 1 1 nil) '(4 8))
(syntax-comment-expect 'lisp-back-comment (syntax-comment-lisp "(x) ; )\n" nil -1 t) '(1 1))
(syntax-comment-expect 'lisp-list-comment (syntax-comment-lisp "(a ; )\n b)" 1 1 t) '(11 11))
(syntax-comment-expect 'property-flags (syntax-comment-property) '(1 13))

(dolist (f (reverse syntax-comment-failures))
  (princ (format "  MISMATCH %S\n" f)))
(princ (format "SYNTAX-COMMENT failures=%d %s\n" (length syntax-comment-failures)
               (if syntax-comment-failures "FAIL" "PASS")))
(kill-emacs (if syntax-comment-failures 1 0))

;;; standalone-syntax-comment-check.el ends here
