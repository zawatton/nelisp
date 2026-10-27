;;; host-parity.el --- word-boundary / non-ASCII native program vs GNU string-match  -*- lexical-binding: t; -*-
;; Runs in GNU Emacs against the Lisp reference VM (tools/ai/rxnative/refvm.el), checking the
;; program compiler (`nlre--nat-compile') and the VM semantics.  Prints "RESULT ok= bad= native=".
(let ((root (expand-file-name "../../.." (file-name-directory load-file-name))))
  (load (expand-file-name "tools/ai/rxnative/host-shim.el" root) nil t))
(defalias 'nlre--nat-program-1 (symbol-function 'nlre--nat-program))
(defvar hp-ok 0) (defvar hp-bad 0) (defvar hp-native 0) (defvar hp-decl 0)
(defvar hp-saw nil)
(defvar hp-lispdiff 0) ; declined cases where the (pre-existing) Lisp matcher differs from GNU; not this lane's path
(let ((orig (symbol-function 'nelisp--rx-exec)))
  (fset 'nelisp--rx-exec
        (lambda (&rest a) (let ((r (apply orig a))) (setq hp-saw (if (eq r t) 'decl 'native)) r))))
;; The string-match route never compiles word assertions (its native rows must stay valid); the buffer
;; routes compile them through `nlre--nat-program-syn' and fill the syntax map before running.  Mimic that
;; for string-match here so the VM and the map are exercised against GNU.
(defvar hp-sm-declines
  (and (fboundp 'nlre--nat-program-syn) (null (nlre--nat-program "\\<beta\\>" nil nil))
       (null (nlre--nat-program "\\bx" t nil)) (nlre--nat-program "beta" nil nil) t))
(when (fboundp 'nlre--nat-program-syn)
  (fset 'nlre--nat-program
        (lambda (re fold literal)
          (if literal (nlre--nat-program-1 re fold t) (nlre--nat-program-syn re fold))))
  (let ((orig-run (symbol-function 'nlre--nat-run)))
    (fset 'nlre--nat-run
          (lambda (entry &rest a)
            (unless (= 0 (logand (aref (car entry) 8) 16)) (nlre--nat-syn-ensure))
            (apply orig-run entry a)))))
(defun hp-md (f)
  (let ((md (funcall f)) (l nil))
    (while md (setq l (cons (and (car md) (cons (car md) (cadr md))) l) md (cddr md)))
    (nreverse l)))
(defun hp-check (re s st &optional label)
  (let* ((g (condition-case e (let ((r (string-match re s st))) (cons r (and r (hp-md #'match-data)))) (error (list 'err (car e)))))
         (_ (setq hp-saw nil))
         (n (condition-case e (let ((r (nlre-string-match re s st)))
                                (cons r (let ((l nil) (i 0))
                                          (while (< i 4) (let ((b (and r (nlre-match-beginning i)))) (setq l (cons (and b (cons b (nlre-match-end i))) l))) (setq i (1+ i)))
                                          (nreverse l))))
              (error (list 'err (car e)))))
         (gm (and (null (eq (car g) 'err)) (cdr g))) )
    (if (eq hp-saw 'native) (setq hp-native (1+ hp-native)) (setq hp-decl (1+ hp-decl)))
    (cond ((eq (car g) 'err) (setq hp-ok (1+ hp-ok)))
          ((and (equal (car g) (car n))
                (let ((ok t) (i 0))
                  (while (and ok (< i 4))
                    (setq ok (equal (nth i gm) (nth i (cdr n))) i (1+ i)))
                  ok))
           (setq hp-ok (1+ hp-ok)))
          ((not (eq hp-saw 'native)) (setq hp-lispdiff (1+ hp-lispdiff)))
          (t (setq hp-bad (1+ hp-bad))
             (princ (format "BAD %S %S st=%S fold=%S %s: gnu=%S nl=%S native=%S\n" re s st case-fold-search (or label "") g n hp-saw))))))
(defvar hp-pats
  '("\\<beta\\>" "\\<b" "a\\>" "\\bfoo\\b" "\\Boo\\B" "\\B" "\\b" "\\<" "\\>" "\\_<" "\\_>"
    "\\_<foo-bar\\_>" "\\_<[a-z]+\\_>" "\\<[a-z]+\\>" "\\(\\<b[a-z]*\\)\\(\\>\\)" "\\<\\(a\\|b\\)+\\>"
    "x\\b" "\\bx" "\\Bx" "x\\B" "\\bx\\b\\|\\Bx" "\\(\\bfoo\\)\\1" "\\<\\w" "foo\\>" "\\<foo" "\\_<foo\\_>"
    "\\_<[[:alpha:]_-]+" "o\\{2\\}\\>" "\\(?:\\<a\\)*b" "\\<a*\\>" "\\Ba\\|\\ba" "\\`\\<" "\\>\\'"
    "\\<日本" "日本語\\>" "c\n日" "\\<日" "日\\>" "c日\\|b" "\\(日\\)本" "a日*" "[日]" "日本\\|語" "x\\(日\\)?y" "あ" "ア\\>" "\\bあ"
    "\\(日本\\)+" "日\\{2\\}" "\\<abc日" "é" "É" "ß" "Σ"))
(defvar hp-strs
  '("" "a" "x" "beta" "beta alpha beta" "xbetax" "beta_beta" "beta-beta" " beta " "foo-bar foo_bar" "foo bar" "foofoo"
    "foo foo" "ab ba abba" "oo foo ooo" "x xx xxx" "a1 b2 $c %d" "  " "!@#" "foo\nbar" "_x_" "-x-" "a.b" "ab" "aab bba" "boo"
    "日本語abc\n日本語abc\n" "xx日本語 foo" "日本語" "日 本" "ab日" "日ab" "a日b" "c\n日" "x日y" "xy" "あいう アイウ" "é É" "foo é bar" "Σσ ß"
    "  beta日" "日beta" "beta日"))
(defun hp-run-table (name)
  (dolist (fold '(nil t))
    (let ((case-fold-search fold))
      (dolist (re hp-pats)
        (dolist (s hp-strs)
          (dolist (st '(0 1 3))
            (when (<= st (length s))
              (hp-check re s st name))))))))
(hp-run-table "standard")
(with-syntax-table emacs-lisp-mode-syntax-table (hp-run-table "elisp"))
(with-syntax-table text-mode-syntax-table (hp-run-table "text"))
(let ((tbl (make-syntax-table)))
  (modify-syntax-entry ?- "w" tbl) (modify-syntax-entry ?_ "w" tbl) (modify-syntax-entry ?o "_" tbl)
  (with-syntax-table tbl (hp-run-table "custom1")
    (modify-syntax-entry ?x "w" tbl)
    (nlre-clear-syntax-cache)
    (hp-run-table "custom1-after-modify")))
;; Legacy ASCII corpus of tools/ai/rxnative/run-ref.el through the same checks (no regression of the old path).
(with-temp-buffer
  (insert-file-contents (expand-file-name "../rxnative/run-ref.el" (file-name-directory load-file-name)))
  (goto-char (point-min))
  (while (re-search-forward "^(defvar rt-\\(pats\\|subs\\)" nil t)
    (goto-char (match-beginning 0)) (eval (read (current-buffer)) t)))
(dolist (fold '(nil t))
  (let ((case-fold-search fold))
    (dolist (re rt-pats) (dolist (s rt-subs) (dolist (st '(0 1)) (when (<= st (length s)) (hp-check re s st "legacy")))))))
;; unibyte haystacks and a multibyte pattern against unibyte text
(dolist (s '("beta beta" "xbeta" "a_b"))
  (dolist (re '("\\<beta\\>" "\\bb" "\\_<a_b\\_>" "\\Bet"))
    (hp-check re (string-to-unibyte s) 0 "unibyte")))
(hp-check "日" (string-to-unibyte "abc") 0 "unibyte-mbpat")
(dolist (s '("beta alpha" "\\<xx"))
  (hp-check "\\<beta" (concat "日本" s) 2 "start-in-mb"))
(princ (format "SM-ROUTE-WORD-DECLINES %S\n" hp-sm-declines))
(princ (format "RESULT ok=%d bad=%d native=%d declined=%d lisp-path-diff=%d\n" hp-ok hp-bad hp-native hp-decl hp-lispdiff))
