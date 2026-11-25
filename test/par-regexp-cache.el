;;; par-regexp-cache.el --- Cached regexp execution -*- lexical-binding: t; -*-

(defun par-regexp-cache-row (label &rest values)
  (prin1 (cons label values))
  (terpri))

(let ((pattern (copy-sequence "\\.tar\\.gz\\'"))
      (case-fold-search t))
  (par-regexp-cache-row
   'absolute-suffix
   (string-match pattern "a.psd")
   (string-match pattern "a.TAR.GZ") (match-data t)
   (string-match pattern "a.tar.gz\n") (match-data t)
   (string-match pattern "a.tar.gz" -7)
   (string-match pattern "a.tar.gz" -6)
   (string-match "other" "other")
   (string-match pattern "a.tar.gz"))
  (aset pattern 7 ?x)
  (par-regexp-cache-row 'mutated-suffix
                       (string-match pattern "a.tar.gz")
                       (string-match pattern "a.tar.xz")
                       (string-match "\\.tar\\.gz\\'" "a.tar.gz")))

(let ((pattern "\\`abc\\([0-9]+\\)") (case-fold-search t))
  (par-regexp-cache-row
   'prefix
   (string-match pattern "ABx12")
   (string-match pattern "ABC12") (match-data t)
   (string-match-p pattern "abc7") (match-data t)
   (string-match pattern "abc8" nil t) (match-data t)
   (let ((case-fold-search nil)) (string-match pattern "ABC12"))
   (string-match "^abc[0-9]" "x\nABC3")
   (string-match "abc[0-9]" "xxabx xxAbC7")))

(let ((pattern "^abc\\([0-9]\\)$") (case-fold-search t))
  (with-temp-buffer
    (insert "abx0\nABC1\nabx2\nabc3\n")
    (goto-char (point-min))
    (let ((forward (re-search-forward pattern nil t)) (first (match-data t)))
      (goto-char (point-max))
      (let ((backward (re-search-backward pattern nil t)) (last (match-data t)))
        (par-regexp-cache-row 'buffer-prefix forward first backward last
                             (looking-at pattern) (match-data t)
                             (looking-at "abc" t) (match-data t)
                             (looking-at "abc4" t) (match-data t))))))

(with-temp-buffer
  (insert "xy abc.gz")
  (let ((pattern "abc\\.gz\\'") (case-fold-search nil))
    (goto-char (point-min))
    (par-regexp-cache-row 'bounded-suffix
                         (re-search-forward pattern (1- (point-max)) t)
                         (point) (re-search-forward pattern nil t)
                         (match-data t) (re-search-backward pattern nil t)
                         (looking-at pattern) (match-data t))))

(with-temp-buffer
  (insert "abc")
  (goto-char 2)
  (par-regexp-cache-row
   'invalid-search-bound
   (condition-case err (re-search-forward "[" 1 t) (error err))
   (point)))

(let ((standard (copy-syntax-table))
      (changed (copy-syntax-table))
      (pattern "ab\\sw\\'"))
  (modify-syntax-entry ?_ "w" changed)
  (par-regexp-cache-row
   'syntax
   (with-syntax-table standard (string-match-p pattern "ab_"))
   (with-syntax-table changed (string-match-p pattern "ab_"))
   (with-syntax-table standard (string-match-p pattern "ab_"))
   (with-syntax-table changed
     (aset changed ?_ (cons 1 nil))
     (string-match-p pattern "ab_"))))

(with-temp-buffer
  (let ((case-fold-search t)
        (table (copy-case-table (current-case-table)))
        (pattern "abc\\'"))
    (par-regexp-cache-row
     'case-table
     (string-match-p pattern "abX")
     (progn (set-case-syntax-pair ?X ?c table) (set-case-table table)
            (string-match-p pattern "abX"))
     (progn (set-case-table (standard-case-table))
            (string-match-p pattern "abX")))))

(with-temp-buffer
  (insert "abX")
  (let ((case-fold-search t)
        (table (copy-case-table (current-case-table)))
        (pattern "\\`abc\\'"))
    (goto-char (point-min))
    (par-regexp-cache-row
     'buffer-case-table
     (looking-at pattern)
     (progn (set-case-syntax-pair ?X ?c table) (set-case-table table)
            (looking-at pattern))
     (re-search-forward pattern nil t)
     (re-search-backward pattern nil t)
     (progn (set-case-table (standard-case-table))
            (looking-at pattern)))))

(let ((pattern "ab cd\\'") (case-fold-search nil))
  (par-regexp-cache-row
   'search-spaces
   (string-match-p pattern "ab---cd")
   (let ((search-spaces-regexp "-+")) (string-match-p pattern "ab---cd"))
   (string-match-p pattern "ab---cd")
   (let ((search-spaces-regexp "[ ]+")) (string-match-p pattern "ab   cd"))))

(let ((pattern (concat (unibyte-string 233) "\\'"))
      (case-fold-search nil))
  (par-regexp-cache-row 'byte-kind
                       (string-match-p pattern (unibyte-string 233))
                       (string-match-p pattern "é")
                       (string-match-p "é\\'" (unibyte-string 233))
                       (string-match-p "é\\'" "😀é")
                       (let ((case-fold-search t))
                         (string-match-p "ask\\'" "😀asK"))))

(let ((pattern (copy-sequence "abc")))
  (string-match "a" "za")
  (par-regexp-cache-row
   'invalid-after-warm (string-match-p pattern "abc")
   (progn (aset pattern 0 ?\[)
          (condition-case err (string-match pattern "abc")
            (invalid-regexp err)))
   (condition-case err (string-match-p pattern "abc")
     (invalid-regexp err))
   (match-data t)))

(par-regexp-cache-row
 'assoc-default
 (assoc-default "ab" '(("z" . miss) ("a" . first) ("ab" . second)) #'string-match)
 (assoc-default 'a '(a (a . shadow)) nil 'atom)
 (assoc-default 'a '((a) (a . shadow)))
 (assoc-delete-all 'a (list (cons 'a 1) 'atom (cons 'a 2)))
 (assq-delete-all 'a (list (cons 'a 1) 'atom (cons 'a 2)))
 (or (not (fboundp 'nelisp--rx-exec))
     (and (byte-code-function-p (symbol-function 'assoc-default))
          (byte-code-function-p (symbol-function 'assoc-delete-all))
          (byte-code-function-p (symbol-function 'assq-delete-all)))))

(par-regexp-cache-row
 'cache-entry-reused
 (or (not (fboundp 'nelisp--rx-exec))
     (let* ((pattern (copy-sequence "\\`abc"))
            (program (nlre--nat-program pattern nil nil))
            (entry nlre--nat-last))
       (nlre--nat-program "xyz" nil nil)
       (and program
            (eq program (nlre--nat-program (copy-sequence pattern) nil nil))
            (eq entry nlre--nat-last)))))
