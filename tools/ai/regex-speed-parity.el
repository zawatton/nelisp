;;; regex-speed-parity.el --- GNU/standalone public API corpus -*- lexical-binding: nil; -*-
(when (getenv "REGEX_SPEED_MODULE")
  (load (getenv "REGEX_SPEED_MODULE"))
  (nlre--compiled-cache-clear))

(defun regex-speed-data ()
  (mapcar (lambda (item)
            (cond ((markerp item) (marker-position item))
                  ((bufferp item) 'buffer)
                  (t item)))
          (match-data t)))

(defun regex-speed-case (name form)
  (let ((answer (condition-case error-data (eval form)
                  (error (list :error error-data)))))
    (princ (format "%S %S\n" name answer))))

(dolist (fold '(nil t))
  (let ((case-fold-search fold))
    (dolist (pattern '("file" "" "\\`\\(.*\\)\\(\\)\\'"
                       "\\`\\(.*\\)\\(\\.el\\)\\'"
                       "\\`\\(.*\\)\\(a\\|ba\\)\\'"
                       "\\`\\(.*\\)\\(\\.so\\|\\.elc\\|\\.elc\\.gz\\|\\.el\\|\\.el\\.gz\\)\\'"
                       "\\(a+\\)\\1" "\\w+" "\\sw+" "[[:alpha:]]+" "ä" "σ"))
      (dolist (text '("some-file.el" "some-FILE.EL" "" "ba" "aabbaa"
                      "a\nb.el" "some.el.gz" "Ää σς" "___"))
        (let ((answer (string-match pattern text)))
          (princ (format "match %S %S %S %S %S\n"
                         fold pattern text answer (and answer (regex-speed-data)))))))))

(regex-speed-case 'predicate-preserves-data
 '(progn (string-match "\\(sentinel\\)" "sentinel")
         (let ((before (regex-speed-data)))
           (list (string-match-p "file" "some-file.el")
                 (equal before (regex-speed-data))))))
(regex-speed-case 'raw-bytes
 '(let* ((case-fold-search nil) (s (unibyte-string 255 97 0 98)))
    (list (string-match (unibyte-string 255) s) (regex-speed-data)
          (string-match "a.b" s) (regex-speed-data))))
(regex-speed-case 'mutable-pattern
 '(let ((pattern (copy-sequence "abc")) (case-fold-search nil))
    (string-match pattern "abc") (aset pattern 0 ?x)
    (list (string-match pattern "abc") (string-match pattern "xbc"))))
(regex-speed-case 'syntax-table
 '(with-temp-buffer
    (set-syntax-table (copy-syntax-table))
    (modify-syntax-entry ?_ "w")
    (list (string-match "\\w+" "___") (regex-speed-data))))
(regex-speed-case 'space-translation
 '(let ((search-spaces-regexp "[ \t]+"))
    (list (string-match "a b" "a\tb") (regex-speed-data))))
(regex-speed-case 'negative-start
 '(list (string-match "a" "ba" -1) (regex-speed-data)))

(regex-speed-case 'unibyte-space-substitution
 '(let ((case-fold-search nil) results)
    (dolist (spaces (list (string 255) (unibyte-string 255)
                         (string-to-multibyte (unibyte-string 128))
                         (string-to-multibyte (unibyte-string 255))))
      (let ((search-spaces-regexp spaces))
        (dolist (text (list (unibyte-string 255) (string 255)
                           (unibyte-string 195 191) (unibyte-string 192 128)
                           (unibyte-string 193 191)))
          (let ((answer (string-match (unibyte-string 32) text)))
            (setq results (cons (list answer (and answer (regex-speed-data)))
                                results))))))
    (nreverse results)))

(regex-speed-case 'anchored-byte-fastmap
 '(let ((case-fold-search t) (search-spaces-regexp (string 255)))
    (list (string-match "\\( \\)" (unibyte-string 195 191)) (regex-speed-data)
          (string-match "\\`\\( \\)\\'" (unibyte-string 195 191))
          (string-match "\\`\\( \\)\\'"
                        (string-to-multibyte (unibyte-string 195 191)))
          (regex-speed-data))))

(dolist (form '((string-match "" "abc" -5)
                (string-match-p "" "abc" -5)
                (string-match "[" "a")
                (string-match "\\(" "a")
                (string-match "\\)" "a")
                (string-match "\\" "a")
                (progn (string-match "x" "x") (match-beginning -1))
                (with-temp-buffer (looking-at "["))
                (with-temp-buffer (re-search-forward "[" nil t))))
  (regex-speed-case (list 'error-data form) form))

(dolist (pattern '("file" "\\(f.+?\\)e" "" "^" "\\w+"))
  (regex-speed-case (list 'looking pattern)
   `(with-temp-buffer
      (insert "file file\n") (goto-char 1)
      (let ((forward (looking-at ,pattern)))
        (goto-char 5)
        (list forward (looking-back ,pattern 1 t)
              (regex-speed-data)))))
  (dolist (direction '(forward backward))
    (regex-speed-case (list 'buffer direction pattern)
     `(with-temp-buffer
        (insert "file file\n")
        (goto-char ,(if (eq direction 'forward) 1 11))
        (let ((answer (,(if (eq direction 'forward) 're-search-forward 're-search-backward)
                       ,pattern nil t)))
          (list answer (point) (and answer (regex-speed-data))))))))
(regex-speed-case 'replacement
 '(list (replace-regexp-in-string "\\(file\\)" "[\\1]" "file file")
        (replace-regexp-in-string "" "X" "ab")
        (replace-regexp-in-string "ä" "x" "Ää")))
(dolist (fold '(nil t))
  (let ((case-fold-search fold))
    (dolist (class '("alpha" "alnum" "word" "upper" "lower" "space"
                     "blank" "punct" "cntrl" "graph" "print"))
      (let ((pattern (concat "[[:" class ":]]")) answers)
        (dolist (char '(9 10 32 65 95 97 127 160 178 223 453 769
                       8544 19968 8203 128512 1114111))
          (setq answers
                (cons (and (string-match pattern (char-to-string char)) t)
                      answers)))
        (princ (format "class %S %S %S\n" fold class (nreverse answers)))))))
(regex-speed-case 'unicode-replacement-case
 '(list (replace-regexp-in-string "Ab" "ǆ" "Ab")
        (replace-regexp-in-string "Ab" "ﬃ" "Ab")
        (replace-regexp-in-string "AB" "ß" "AB")
        (replace-regexp-in-string "AB" "İ" "AB" t)))
(regex-speed-case 'custom-case-table
 '(progn
    (when (fboundp 'nelisp--eval-source-string) (require 'emacs-char-table))
    (require 'case-table)
    (let ((saved (current-case-table)) (case-fold-search t))
      (unwind-protect
          (let ((table (copy-case-table saved)))
            (set-case-syntax-pair ?A ?/ table)
            (set-case-table table)
            (list (string-match "A" "/") (string-match "[A]+" "/")
                  (string-match "[[:upper:]]" "/")))
        (set-case-table saved)))))
(regex-speed-case 'raw-set-kinds
 '(let ((case-fold-search nil) (search-spaces-regexp "-"))
    (list (string-match (unibyte-string ?\[ 255 ?\]) (unibyte-string 255))
          (string-match (unibyte-string ?\[ 255 ?\]) (string 255))
          (string-match "[[:word:]]" (unibyte-string 255)))))
(regex-speed-case 'raw-folded-backreference
 '(let ((case-fold-search t))
    (list (string-match "\\(.\\)\\1" (unibyte-string 201 233))
          (string-match "\\(.\\)\\1" (unibyte-string 233 233)))))
(regex-speed-case 'custom-cross-byte-fold
 '(let ((saved (current-case-table)) (case-fold-search t))
    (unwind-protect
        (let* ((table (copy-case-table saved))
               (_installed (set-case-table table))
               (canonical (if (fboundp 'nelisp-casetab--get-extra-slot)
                              (nelisp-casetab--get-extra-slot table 1)
                            (char-table-extra-slot table 1)))
               (multi-a (string-to-multibyte "A")))
          (if (and (fboundp 'emacs-char-table-p) (emacs-char-table-p canonical))
              (emacs-char-table-set canonical ?A ?λ)
            (aset canonical ?A ?λ))
          (list (string-match "[[:lower:]]" "A")
                (string-match "x[[:lower:]]" "xA")
                (string-match "[[:lower:]]" multi-a)
                (string-match "A" "λ") (string-match multi-a "λ")
                (string-match "[A]" "A") (string-match "[A]" "λ")))
      (set-case-table saved))))
(dolist (row '(("\\(ab\\|a\\)*b" "ab")
                  ("\\(ab\\|a\\)+b" "ab")
                  ("\\(ab\\|a\\)?b" "ab")
                  ("\\(ab\\|a\\)\\{1,2\\}b" "ab")
                  ("\\(ab\\|a\\)*?b" "ab")
                  ("\\(ab\\|a\\)+?b" "ab")
                  ("\\(\\(ab\\|a\\)\\)*b" "aab")
                  ("\\(\\(a\\|b\\)*\\)\\2" "ab")
                  ("\\(a\\|b\\)*\\1" "abb")
                  ("\\(a\\|b\\)*\\1" "aba")
                  ("\\(a?\\)*" "") ("\\(a?\\)*" "aa")
                  ("\\(a*\\)+" "aa")
                  ("\\(ab\\|a\\)?b" "b")
                  ("\\(\\(a\\)\\|\\(b\\)\\)*c" "abc")
                  ("\\(a\\(b\\)?\\)*" "aba")
                  ("\\(a?\\|b\\)\\{0,2\\}a" "ba")
                  ("\\(a?\\|b\\)\\{0,2\\}a" "baa")
                  ("\\(a?\\|b\\)\\{1,3\\}a" "ba")
                  ("\\(a?\\|b\\)\\{2,4\\}a" "ba")))
  (regex-speed-case (list 'general-backtracking row)
   `(let* ((case-fold-search nil)
           (hit (string-match ,(car row) ,(cadr row))))
      (list hit (and hit (regex-speed-data))))))
(regex-speed-case 'smart-loop-overlap-and-unicode
 '(let (results)
    (dolist (fold '(nil t))
      (let ((case-fold-search fold))
        (dolist (row '(("\\`a*b\\|c\\'" "aaaac")
                       ("\\`\\(a*\\)a\\'" "aaa")
                       ("\\`\\([^k]*\\)k\\'" "Kk")
                       ("\\`\\([A-Z]*\\)b\\'" "Kb")
                       ("\\`\\([[:alnum:]]*\\):\\'" "abc:")
                       ("\\`\\(.*\\)\n\\'" "aaa\n")))
          (let ((hit (string-match (car row) (cadr row))))
            (setq results (cons (list fold row hit (and hit (regex-speed-data))) results))))))
    (nreverse results)))
(princ "REGEX-PARITY-DONE\n")
