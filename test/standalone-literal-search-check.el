;;; standalone-literal-search-check.el --- literal search-forward and split-string -*- lexical-binding: t -*-

;; Usage: nelisp --load test/standalone-literal-search-check.el
;;
;; `search-forward' went through `regexp-quote' and the interpreted regexp
;; matcher (66 s for the 2000 ` d="' of a 709 KB SVG), and `split-string'
;; on a literal separator through the same matcher (13 s for its lines).
;; Both now scan natively when there is nothing for the matcher to do.
;; Each case is what GNU Emacs 31.1 answers.  Prints one LITERAL-SEARCH
;; line and exits non-zero on failure.

(defun literal-search-run ()
  (let (out)
    (with-temp-buffer
      (insert "ab d=\"1\" φ d=\"2\" ABC abc\nx")
      (goto-char 1)
      (push (list (search-forward " d=\"" nil t) (point) (match-beginning 0) (match-end 0)) out)
      (push (list (search-forward " d=\"") (match-beginning 0)) out)
      (push (search-forward " d=\"" nil t) out)
      (push (point) out)
      (goto-char 1)
      (push (condition-case e (search-forward "zz") (error e)) out)
      (push (list (search-forward "zz" nil 'move) (point)) out)
      (goto-char 1)
      (push (list (search-forward "φ" 12 t) (point)) out)
      (push (list (search-forward "φ" 13 t) (point)) out)
      (goto-char 5)
      (push (condition-case e (search-forward "a" 3) (error (car e))) out)
      (goto-char 1)
      (let ((case-fold-search t)) (push (list (search-forward "abc" nil t) (match-beginning 0)) out))
      (goto-char 1)
      (let ((case-fold-search nil)) (push (list (search-forward "abc" nil t) (match-beginning 0)) out))
      (goto-char 1)
      (push (list (search-forward "" nil t) (point)) out)
      (goto-char 1)
      (push (list (search-forward "d" nil t 2) (point)) out)
      (narrow-to-region 1 10)
      (goto-char 1)
      (push (list (search-forward "φ" nil t) (point)) out)
      (widen))
    (dolist (c '(("a\nb\n\nc\n" "\n") ("a\nb\n\nc\n" "\n" t) ("\ta\tb" "\t") ("x,,y" ",")
                 ("x,,y" "," t) ("" ",") ("" "," t) ("a::b::c" "::") ("日本,語" ",")
                 ("a.b.c" ".") ("a.b.c" "\\.") ("a  b" " +") ("  a b  " nil)))
      (push (apply #'split-string c) out))
    (nreverse out)))

(let* ((got (literal-search-run))
       (want '((7 7 3 7) (15 11) nil 15 (search-failed "zz") (nil 27) (11 11) (nil 11) error (21 18) (25 22) (1 1) (13 13) (nil 1) ("a" "b" "" "c" "") ("a" "b" "c") ("" "a" "b") ("x" "" "y") ("x" "y") ("") nil ("a" "b" "c") ("日本" "語") ("" "" "" "" "" "") ("a" "b" "c") ("a" "b") ("a" "b")))
       (ok (equal got want)))
  (unless ok
    (let ((g got) (w want) (i 0))
      (while (or g w)
        (unless (equal (car g) (car w)) (princ (format "  MISMATCH %d got=%S want=%S\n" i (car g) (car w))))
        (setq g (cdr g) w (cdr w) i (1+ i)))))
  (princ (format "LITERAL-SEARCH %s\n" (if ok "PASS" "FAIL")))
  (kill-emacs (if ok 0 1)))

;;; standalone-literal-search-check.el ends here
