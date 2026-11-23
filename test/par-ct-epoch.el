;;; -*- lexical-binding: t; -*-
;; Char-table write epoch (F415).  After each kind of char-table write (raw aset,
;; set-char-table-range on a char and on a range, set-char-table-parent, a write to the
;; parent of a child table, modify-syntax-entry, the standard table) the syntax-dependent
;; atoms \w \sw \s_ \S- \< \> \_< \b must answer as GNU does through string-match and
;; looking-at.  Spec = GNU 31.1 (par-ct-epoch.gnu).  Every row warms the path first.
(set-syntax-table (standard-syntax-table))
(defmacro row (n f) `(princ (format "%-26s %S\n" ,n (condition-case e ,f (error (list 'ERR (car e)))))))
(defvar ce-pats '("\\w" "\\sw" "\\s_" "\\S-" "\\<" "\\>" "\\_<" "\\b"))
(defvar ce-texts '("_a" "-_" "a_b-c" "x _y"))
(defun ce-probe ()
  "Results of every pattern on every text, by string-match and by looking-at."
  (let ((st (syntax-table)))
    (list
     (mapcar (lambda (re) (mapcar (lambda (s) (string-match re s)) ce-texts)) ce-pats)
     (mapcar (lambda (re)
               (mapcar (lambda (s)
                         (with-temp-buffer
                           (set-syntax-table st) (insert s)
                           (mapcar (lambda (p) (goto-char p) (and (looking-at re) (match-end 0)))
                                   (list 1 2 3 (point-max)))))
                       ce-texts))
             ce-pats))))
(defun ce-fresh () (make-syntax-table))
(defun ce-jp ()
  "Japanese text: bytes >= 128 are read from the live table."
  (list (string-match "\\w+" "あい_foo") (string-match "\\s_" "あ_x") (string-match "\\s_" "あ")
        (string-match "\\W" "あい foo") (string-match "\\w" "_い")
        (with-temp-buffer (set-syntax-table (syntax-table)) (insert "あい_foo") (goto-char 1)
                          (list (looking-at "\\w") (looking-at "\\s_") (progn (goto-char 3) (looking-at "\\w+"))))))
(row "E1 raw aset"
     (let ((st (ce-fresh)))
       (with-syntax-table st
         (let ((a (ce-probe)))
           (aset st ?_ (cons 2 nil))
           (let ((b (ce-probe)))
             (aset st ?- (cons 3 nil))
             (let ((c (ce-probe)))
               (aset st ?_ (cons 1 nil))
               (list a b c (ce-probe))))))))
(row "E2 set-range char"
     (let ((st (ce-fresh)))
       (with-syntax-table st
         (let ((a (ce-probe)))
           (set-char-table-range st ?_ (cons 2 nil))
           (let ((b (ce-probe)))
             (set-char-table-range st ?_ (cons 1 nil))
             (list a b (ce-probe)))))))
(row "E3 set-range range"
     (let ((st (ce-fresh)))
       (with-syntax-table st
         (let ((a (ce-probe)))
           (set-char-table-range st '(?! . ?/) (cons 2 nil))
           (let ((b (ce-probe)))
             (set-char-table-range st '(?a . ?c) (cons 1 nil))
             (list a b (ce-probe)))))))
(row "E4 set-parent swap"
     (let* ((p1 (ce-fresh)) (p2 (ce-fresh)) (kid (make-char-table 'syntax-table nil)))
       (aset p2 ?_ (cons 2 nil))
       (set-char-table-parent kid p1)
       (with-syntax-table kid
         (let ((a (ce-probe)))
           (set-char-table-parent kid p2)
           (let ((b (ce-probe)))
             (set-char-table-parent kid p1)
             (list a b (ce-probe)))))))
(row "E5 parent write"
     (let* ((par (ce-fresh)) (kid (make-syntax-table par)))
       (with-syntax-table kid
         (let ((a (ce-probe)))
           (aset par ?_ (cons 2 nil))
           (let ((b (ce-probe)))
             (set-char-table-range par ?- (cons 2 nil))
             (list a b (ce-probe)))))))
(row "E6 modify-syntax-entry"
     (let ((st (ce-fresh)))
       (with-syntax-table st
         (let ((a (ce-probe)))
           (modify-syntax-entry ?_ "w" st)
           (let ((b (ce-probe)))
             (modify-syntax-entry '(?a . ?c) "_" st)
             (list a b (ce-probe)))))))
(row "E7 japanese"
     (let ((st (ce-fresh)))
       (with-syntax-table st
         (let ((a (ce-jp)))
           (aset st ?あ (cons 3 nil))
           (let ((b (ce-jp)))
             (set-char-table-range st '(?あ . ?う) (cons 1 nil))
             (let ((c (ce-jp)))
               (aset st ?_ (cons 2 nil))
               (list a b c (ce-jp) (ce-probe))))))))
(row "E8 unrelated tables"
     (let ((st (ce-fresh)))
       (with-syntax-table st
         (let ((a (ce-probe)) (other (make-char-table 'foo)) (other2 (ce-fresh)))
           (aset other ?a 1) (set-char-table-range other '(?a . ?z) 2)
           (aset other2 ?_ (cons 2 nil))
           (set-char-table-parent other other2)
           (let ((b (ce-probe)))
             (aset other ?b 3)
             (list a b (ce-probe) (equal a b)))))))
(row "E9 standard table"
     (let ((old (aref (standard-syntax-table) ?_)) (st (ce-fresh)))
       (unwind-protect
           (with-syntax-table st
             (let ((a (ce-probe)))
               (aset (standard-syntax-table) ?_ (cons 2 nil))
               (let ((b (ce-probe)))
                 (aset (standard-syntax-table) ?_ old)
                 (list a b (ce-probe)))))
         (aset (standard-syntax-table) ?_ old))))
(defun ce-scan (re)
  (goto-char 1)
  (let (r (go t))
    (while (and go (< (length r) 20) (re-search-forward re nil t))
      (push (point) r)
      (when (= (match-beginning 0) (point))
        (if (eobp) (setq go nil) (forward-char 1))))
    (nreverse r)))
(row "E10 re-search"
     (let ((st (ce-fresh)))
       (with-syntax-table st
         (with-temp-buffer
           (set-syntax-table st) (insert "a_b c_d")
           (let ((a (mapcar #'ce-scan ce-pats)))
             (aset st ?_ (cons 2 nil))
             (let ((b (mapcar #'ce-scan ce-pats)))
               (modify-syntax-entry ?c "_" st)
               (list a b (mapcar #'ce-scan ce-pats))))))))
