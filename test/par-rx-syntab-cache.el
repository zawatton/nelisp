;;; -*- lexical-binding: t; -*-
;; \w \s_ matches across syntax-table changes (D3 cache of nlre--setup-syntax-table).  Spec = GNU 31.1 (par-rx-syntab-cache.gnu).
(defmacro row (n f) `(princ (format "%-26s %S\n" ,n (condition-case e ,f (error (list 'ERR (car e)))))))
(defun sc-probe (&optional buf-text)
  "Match results for _ - and a under the current syntax table."
  (list (string-match "\\w" "_") (string-match "\\s_" "_") (string-match "\\w" "-")
        (string-match "\\s_" "-") (string-match "\\w+" "a_b")
        (and (string-match "\\w+" "a_b") (match-end 0))
        (let ((st (syntax-table)))
         (with-temp-buffer
          (set-syntax-table st)
          (insert (or buf-text "_-a")) (goto-char 1)
          (let ((r1 (looking-at "\\w")) (r2 (looking-at "\\s_")))
            (goto-char 2)
            (list r1 r2 (looking-at "\\w") (looking-at "\\s_")))))))
(defun sc-fresh () (let ((s (make-syntax-table))) s))
;; R1 in-place modify-syntax-entry between matches
(row "R1 modify in place"
     (let ((st (sc-fresh)))
       (with-syntax-table st
         (let ((a (sc-probe))) (modify-syntax-entry ?_ "w" st)
              (let ((b (sc-probe))) (modify-syntax-entry ?- "_" st)
                   (let ((c (sc-probe))) (modify-syntax-entry ?_ "_" st) (list a b c (sc-probe))))))))
;; R2 raw aset on the table
(row "R2 raw aset"
     (let ((st (sc-fresh)))
       (with-syntax-table st
         (let ((a (sc-probe))) (aset st ?_ (cons 2 nil))
              (let ((b (sc-probe))) (aset st ?- (cons 3 nil)) (list a b (sc-probe)))))))
;; R3 parent table edit (child inherits)
(row "R3 parent edit"
     (let* ((par (make-syntax-table)) (kid (make-syntax-table par)))
       (with-syntax-table kid
         (let ((a (sc-probe))) (modify-syntax-entry ?_ "w" par)
              (let ((b (sc-probe))) (modify-syntax-entry ?_ "." par) (list a b (sc-probe)))))))
;; R4 set-syntax-table
(row "R4 set-syntax-table"
     (with-temp-buffer
       (let ((s1 (sc-fresh)) (s2 (sc-fresh)))
         (modify-syntax-entry ?_ "w" s2)
         (let ((a (progn (set-syntax-table s1) (sc-probe)))
               (b (progn (set-syntax-table s2) (sc-probe)))
               (c (progn (set-syntax-table s1) (sc-probe))))
           (list a b c)))))
;; R5 two buffers with different tables
(row "R5 two buffers"
     (let ((b1 (generate-new-buffer "sc1")) (b2 (generate-new-buffer "sc2")) (s2 (sc-fresh)) r)
       (modify-syntax-entry ?_ "w" s2)
       (with-current-buffer b2 (set-syntax-table s2))
       (dotimes (_ 2)
         (push (with-current-buffer b1 (sc-probe)) r)
         (push (with-current-buffer b2 (sc-probe)) r))
       (kill-buffer b1) (kill-buffer b2) (nreverse r)))
;; R6 with-syntax-table nesting
(row "R6 with-syntax-table"
     (let ((s2 (sc-fresh)) (s3 (sc-fresh)))
       (modify-syntax-entry ?_ "w" s2) (modify-syntax-entry ?- "w" s3)
       (list (sc-probe)
             (with-syntax-table s2 (list (sc-probe) (with-syntax-table s3 (sc-probe)) (sc-probe)))
             (sc-probe))))
;; R7 standard table in-place edit then restore
(row "R7 std table edit"
     (let ((old (aref (syntax-table) ?_)))
       (unwind-protect
           (let ((a (sc-probe))) (modify-syntax-entry ?_ "w") (list a (sc-probe)))
         (modify-syntax-entry ?_ "_") (ignore old))))
;; R8 non-ASCII under a modified table
(row "R8 non-ascii"
     (let ((st (sc-fresh)))
       (with-syntax-table st
         (list (string-match "\\w" "あ") (string-match "\\s_" "あ")
               (progn (modify-syntax-entry ?あ "_" st) (list (string-match "\\w" "あ") (string-match "\\s_" "あ")))))))
