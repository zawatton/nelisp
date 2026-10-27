;;; parity-corpus.el --- word/symbol boundary and non-ASCII literal regexp corpus  -*- lexical-binding: t; -*-
;; Runs unchanged on NeLisp and GNU Emacs; prints one line per case.  Compare the two outputs
;; (tools/ai/rxword/parity.sh).  Pairs of CJK and Latin text next to a boundary are left out:
;; GNU splits words at script changes (WORD_BOUNDARY_P), a separate, older NeLisp gap.
(defvar pc-n 0)
(defun pc-md ()
  (let ((md (match-data)) (l nil))
    (while md (setq l (cons (and (car md) (cons (car md) (cadr md))) l) md (cddr md)))
    (nreverse l)))
(defun pc-emit (tag r) (setq pc-n (1+ pc-n)) (princ (format "%d %s %S\n" pc-n tag r)))
(defun pc-mixed-p (s)
  "Non-nil when S has a non-ASCII character next to an ASCII word-ish one."
  (let ((i 0) (n (length s)) hit)
    (while (and (not hit) (< (1+ i) n))
      (let ((a (aref s i)) (b (aref s (1+ i))))
        (when (or (and (< a 128) (>= b 128) (string-match-p "[A-Za-z0-9_$%-]" (string a)))
                  (and (>= a 128) (< b 128) (string-match-p "[A-Za-z0-9_$%-]" (string b))))
          (setq hit t)))
      (setq i (1+ i)))
    hit))
(defun pc-boundary-p (re) (string-match-p "\\\\[<>bB_]" re))
(defvar pc-skip-odd nil
  "Non-nil in the standard-table pass: skip strings with .!@$%, whose class in NeLisp's
standard syntax table differs from GNU's (a separate gap in the syntax tables, not the matcher).")
(defun pc-sm (re s st)
  (unless (or (and (pc-boundary-p re) (pc-mixed-p s))
              (and pc-skip-odd (pc-boundary-p re) (string-match-p "[.!@$%]" s)))
    (pc-sm1 re s st)))
(defun pc-sm1 (re s st)
  (condition-case e (let ((r (string-match re s st))) (pc-emit (format "sm %S %S %S fold=%S" re s st case-fold-search) (and r (cons r (pc-md)))))
    (error (pc-emit (format "sm %S %S %S" re s st) (list 'err (car e))))))
(defvar pc-pats
  '("\\<beta\\>" "\\<b" "a\\>" "\\bfoo\\b" "\\Boo\\B" "\\B" "\\b" "\\<" "\\>" "\\_<" "\\_>"
    "\\_<foo-bar\\_>" "\\_<[a-z]+\\_>" "\\<[a-z]+\\>" "\\(\\<b[a-z]*\\)\\(\\>\\)" "\\<\\(a\\|b\\)+\\>"
    "x\\b" "\\bx" "\\Bx" "x\\B" "\\bx\\b\\|\\Bx" "\\(\\bfoo\\)\\1" "\\<\\w" "foo\\>" "\\<foo" "\\_<foo\\_>"
    "\\_<[[:alpha:]_-]+" "o\\{2\\}\\>" "\\(?:\\<a\\)*b" "\\<a*\\>" "\\Ba\\|\\ba" "\\`\\<" "\\>\\'"
    "c\n日" "c日\\|b" "\\(日\\)本" "a日*" "[日]" "日本\\|語" "x\\(日\\)?y" "あ" "ア" "\\(日本\\)+" "日\\{2\\}" "日本語" "語a" "é" "É" "ß" "Σ" "σ"))
(defvar pc-strs
  '("" "a" "x" "beta" "beta alpha beta" "xbetax" "beta_beta" "beta-beta" " beta " "foo-bar foo_bar" "foo bar" "foofoo"
    "foo foo" "ab ba abba" "oo foo ooo" "x xx xxx" "a1 b2 $c %d" "  " "!@#" "foo\nbar" "_x_" "-x-" "a.b" "ab" "aab bba" "boo"
    "日本語abc\n日本語abc\n" "日本語 foo" "日本語" "日 本" "c\n日" "x日y" "xy" "あいう アイウ" "é É" "foo é bar" "Σσ ß" "ab 日本語 c"))
(defun pc-buf-all (re s)
  "All matches of RE in a buffer holding S, stepping over empty ones; plus `looking-at' at 1 and 3."
  (with-temp-buffer
    (insert s) (goto-char (point-min))
    (let ((l nil) (n 0) r)
      (while (and (< n 50) (setq r (condition-case nil (re-search-forward re nil t) (error nil))))
        (setq n (1+ n))
        (push (list r (match-beginning 0) (match-end 0) (and (match-beginning 1) (match-beginning 1))) l)
        (when (= (match-beginning 0) (match-end 0))
          (if (eobp) (setq r nil n 50) (forward-char 1))))
      (goto-char (point-min))
      (let ((la1 (condition-case nil (and (looking-at re) (match-end 0)) (error 'err))))
        (goto-char (min 3 (point-max)))
        (list (nreverse l) la1 (condition-case nil (and (looking-at re) (match-end 0)) (error 'err)))))))
(defun pc-bufcase (re s)
  (unless (or (and (pc-boundary-p re) (pc-mixed-p s))
              (and pc-skip-odd (pc-boundary-p re) (string-match-p "[.!@$%]" s)))
    (pc-emit (format "bufall %S %S fold=%S" re s case-fold-search) (pc-buf-all re s))))
(defun pc-run-table (name)
  (princ (format ";; table %s\n" name))
  (dolist (fold '(nil t))
    (let ((case-fold-search fold))
      (dolist (re pc-pats)
        (dolist (s pc-strs)
          (pc-bufcase re s)
          (dolist (st '(0 1 3))
            (when (<= st (length s)) (pc-sm re s st))))))))
(let ((pc-skip-odd t)) (pc-run-table "standard"))
(let ((tbl (make-syntax-table)))
  (dolist (c (string-to-list ".!@$%")) (modify-syntax-entry c "_" tbl))
  (with-syntax-table tbl (pc-run-table "gnu-normalized")))
(let ((tbl (make-syntax-table)))
  (dolist (c (string-to-list ".!@$%")) (modify-syntax-entry c "_" tbl))
  (modify-syntax-entry ?_ "_" tbl) (modify-syntax-entry ?- "_" tbl)
  (with-syntax-table tbl (pc-run-table "elisp-like")))
(let ((tbl (make-syntax-table)))
  (dolist (c (string-to-list ".!@$%")) (modify-syntax-entry c "_" tbl))
  (modify-syntax-entry ?- "w" tbl) (modify-syntax-entry ?_ "w" tbl) (modify-syntax-entry ?o "_" tbl)
  (with-syntax-table tbl
    (pc-run-table "custom1")
    (modify-syntax-entry ?x "w" tbl) (modify-syntax-entry ?b "." tbl)
    (pc-run-table "custom1-after-modify")))
(let ((pc-skip-odd t)) (pc-run-table "standard-again"))
;; unibyte haystacks
(dolist (s '("beta beta" "xbeta" "a_b"))
  (dolist (re '("\\<beta\\>" "\\bb" "\\_<a_b\\_>" "\\Bet"))
    (pc-sm re (string-to-unibyte s) 0)))
(pc-sm "日" (string-to-unibyte "abc") 0)
(pc-sm "\\<beta" (concat "日本 " "beta alpha") 3)
;; buffers: forward loops with bound, narrowing, point restore, looking-at
(defun pc-buf (text re &optional bound)
  (with-temp-buffer
    (insert text) (goto-char (point-min))
    (let (l n)
      (while (setq n (re-search-forward re bound t)) (push (list n (match-beginning 0) (match-end 0)) l))
      (list (nreverse l) (point)))))
(dolist (fold '(nil t))
  (let ((case-fold-search fold))
    (dolist (re '("\\<beta\\>" "\\bfoo\\b" "\\Bo\\B" "\\_<x-y\\_>" "\\<日本語\\>" "c\n日" "語a" "日本"))
      (dolist (text '("beta betax xbeta beta\nfoo foo-bar foofoo x-y x_y\n" "日本語 beta 日本語 abc\n日本語 foo\n" "  beta  beta"))
        (pc-emit (format "buf %S %S fold=%S" re text fold) (pc-buf text re))
        (pc-emit (format "buf-bound %S %S" re text) (pc-buf text re 12))))))
(with-temp-buffer
  (insert "foo bar baz foo") (narrow-to-region 5 12) (goto-char 5)
  (pc-emit "narrow-wordbeg" (list (looking-at "\\<bar\\>") (progn (goto-char 5) (re-search-forward "\\bba[rz]\\b" nil t)) (match-data))))
(with-temp-buffer
  (insert "alpha beta") (goto-char 7)
  (pc-emit "la" (list (looking-at "\\<beta\\>") (looking-at "\\Beta") (progn (goto-char 8) (looking-at "\\Beta")) (progn (goto-char 8) (looking-at "\\bet")))))
(princ (format ";; cases %d\n" pc-n))
(kill-emacs 0)
