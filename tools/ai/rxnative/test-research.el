;;; test-research.el --- F418: re-search routing with class atoms, host refvm vs GNU  -*- lexical-binding: t; -*-
;; Run: emacs --batch -Q -l tools/ai/rxnative/test-research.el
;; Prints RESEARCH ok=N bad=0 native=M.  The native path is exercised through a fake
;; `nelisp-buffer' vector (slots 7 8 narrowing, 11 tick, 12 point, 13 blen, 14 len, 15
;; text memo) as `nlre--nat-bsearch' reads it, with `nelisp--rx-exec' served by refvm.
;; A call counts as native when the matcher ran and never declined; every call is checked
;; against GNU whichever path it took.  Control: RX_MUTANT=back-asc|back-cap runs a mutated
;; copy of `nlre--nat-bsearch-back' and must report bad > 0.
(load (expand-file-name "host-shim.el" (file-name-directory load-file-name)) nil t)
(defvar rr-ok 0) (defvar rr-bad 0) (defvar rr-native 0)
(defvar rr-calls 0) (defvar rr-declined 0)
(advice-add 'nelisp--rx-exec :around
            (lambda (f &rest a)
              (setq rr-calls (1+ rr-calls))
              (let ((r (apply f a))) (when (eq r t) (setq rr-declined (1+ rr-declined))) r)))
(setq nlre--nat-layout t)
(setq parse-sexp-lookup-properties nil)
(let ((m (getenv "RX_MUTANT")))
  (when m
    (let* ((src (with-temp-buffer
                  (insert-file-contents (expand-file-name "lisp/nelisp-stdlib-regexp.el"
                                                          (expand-file-name "../../.." (file-name-directory load-file-name))))
                  (buffer-string)))
           (i (string-match "(defun nlre--nat-bsearch-back" src))
           (form (car (read-from-string src i)))
           (txt (prin1-to-string form)))
      (cond ((equal m "back-asc")
             ;; scan starts upward from LIMIT instead of downward from point
             (setq txt (replace-regexp-in-string "(setq p (1- p))" "(setq p (1+ p))" txt)
                   txt (replace-regexp-in-string "(let ((p pt) (r nil)" "(let ((p limit) (r nil)" txt)
                   txt (replace-regexp-in-string "(>= p limit)" "(<= p pt)" txt)))
            ((equal m "back-cap")
             ;; match end not capped at point
             (setq txt (replace-regexp-in-string "(end (1- pt))" "(end (aref b 14))" txt))))
      (eval (car (read-from-string txt)) t))))
(defun rr-caps ()
  (let ((c nlre--last-caps) (l nil) (i 0))
    (while (< i (length c))
      (setq l (append l (list (and (aref c i) (car (aref c i))) (and (aref c i) (cdr (aref c i))))) i (1+ i)))
    (while (and l (null (car (last l)))) (setq l (butlast l 2)))
    l))
(defun rr-gmd ()
  (let ((md (match-data t)) (l nil))
    (while md (setq l (append l (list (car md) (cadr md))) md (cddr md)))
    (while (and l (null (car (last l)))) (setq l (butlast l 2)))
    l))
(defun rr-fake (text pt)
  (let ((v (make-vector 21 nil)) (n (length text)))
    (aset v 11 1) (aset v 12 (if (= pt (1+ n)) nil pt)) (aset v 13 n) (aset v 14 n)
    (aset v 15 (cons 1 text))
    v))
(defun rr-one (dir re text pt bound noerror count fold)
  (let* ((case-fold-search fold)
         (fn-nl (if (eq dir 'f) #'nlre--re-search-forward-cls #'nlre--re-search-backward))
         (fn-g (if (eq dir 'f) #'re-search-forward #'re-search-backward))
         g n gpt npt)
    (with-temp-buffer
      (insert text) (goto-char pt)
      (set-syntax-table (standard-syntax-table))
      (set-match-data nil)
      (setq g (condition-case e (let ((r (funcall fn-g re bound noerror count))) (list r (point) (and r (rr-gmd))))
                (error (list 'ERR (car e) (point)))))
      (setq gpt (point))
      (goto-char pt)
      (let* ((fb (rr-fake text pt)) (nelisp--current-buffer fb) (c0 rr-calls) (d0 rr-declined))
        (setq nlre--last-caps nil nlre--last-buffer nil)
        (setq n (condition-case e (let ((r (funcall fn-nl re bound noerror count)))
                                    (list r (cond ((not (eql (aref fb 12) (if (= pt (1+ (length text))) nil pt)))
                                                   (or (aref fb 12) (1+ (length text))))
                                                  (t (point)))
                                          (and r (rr-caps))))
                  (error (list 'ERR (car e) (cond ((not (eql (aref fb 12) (if (= pt (1+ (length text))) nil pt)))
                                                   (or (aref fb 12) (1+ (length text))))
                                                  (t (point)))))))
        (when (and (> rr-calls c0) (= rr-declined d0)) (setq rr-native (1+ rr-native)))))
    (if (or (equal g n)
            ;; Known, pre-existing Lisp-path divergence (not this lane): GNU leaves point
            ;; where it was when COUNT > 1 finds fewer matches and signals; the Lisp path
            ;; leaves it at the last match.  Only the error point differs.
            (and (eq (car g) 'ERR) (eq (car n) 'ERR) (equal (butlast g) (butlast n))
                 (integerp count) (> (abs count) 1)))
        (setq rr-ok (1+ rr-ok))
      (setq rr-bad (1+ rr-bad))
      (when (< rr-bad 12)
        (princ (format "BAD %s %S %S pt=%d bound=%S ne=%S count=%S fold=%S\n  gnu=%S\n  nl =%S\n"
                       dir re text pt bound noerror count fold g n))))))
(defvar rr-pats '("\\w" "\\W" "\\s-" "\\S-" "\\sw" "\\s_" "\\s." "\\s(" "\\w+" "\\W+" "\\s-+\\w"
                  "\\(\\w+\\)" "\\(\\w\\)\\(\\s-\\)" "a\\W*b" "\\<\\w" "\\w\\>" "\\_<\\w+" "\\w+\\_>"
                  "\\w\\{2\\}" "x" "ab" "[a-c]+" "\\`\\w" "\\w\\'" "^\\w" "\\w$"
                  "\\(?:foo\\|\\w\\{3\\}\\)" "\\(\\w+\\)\\1"))
(defvar rr-texts '("foo-bar baz_qux (quux) 12.5" "a" "" "  \t x  y" "ab ab" "xx_yy zz" "あい foo" "foo あ bar"))
(defun rr-all ()
  (dolist (re rr-pats)
    (dolist (text rr-texts)
      (let* ((n (length text)) (pts (delete-dups (list 1 (min 2 (1+ n)) (1+ (/ n 2)) (1+ n)))))
        (dolist (pt pts)
          (dolist (fold '(nil t))
            (dolist (dir '(f b))
              (dolist (ne '(nil t other))
                (dolist (count '(nil 1 2 -1))
                  (let ((bounds (if (eq dir 'f)
                                    (list nil (min (1+ n) (+ pt 3)) (1+ n))
                                  (list nil (max 1 (- pt 3)) 1))))
                    (dolist (bd bounds)
                      (rr-one dir re text pt bd ne count fold))))))))))))
(rr-all)
(princ (format "RESEARCH ok=%d bad=%d native=%d\n" rr-ok rr-bad rr-native))
