;;; syntax-rt-semantics.el --- Doc 225 S1.1 fixture: GNU 31.1 syntax-table parity -*- lexical-binding: t; -*-

;; One line per case, NAME => VALUE, compared byte for byte with
;; syntax-rt-semantics.gnu-31.1.expected.  Every case names its syntax
;; table explicitly: the bare runtime's ambient default (the Emacs Lisp
;; table) intentionally differs from a GNU buffer's mode-chosen table.
;; Run: emacs -Q --batch -l FILE, or BIN --load FILE.

(defun srt-show (name thunk)
  (princ (format "%s => %S\n" name
                 (condition-case err (funcall thunk)
                   (error (list 'ERR (car err) (cdr err)))))))

(defmacro srt (name &rest body)
  `(srt-show ,name (lambda () ,@body)))

(defconst srt-chars
  '(0 9 10 12 13 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 57
    58 59 60 61 62 63 64 65 90 91 92 93 94 95 96 97 122 123 124 125 126 127
    128 159 160 161 169 171 172 173 176 183 187 191 215 247 #x2010 #x2018
    #x201c #x2026 #x3000 #x3001 #x300c #x4e00 #xff08 #xff0c #x10000
    #x1f600 #x10ffff #x110000 #x3fff80 #x3fffff))

;;; char-syntax, standard table

(srt "char-syntax/standard"
     (with-syntax-table (standard-syntax-table)
       (concat (mapcar #'char-syntax srt-chars))))
(srt "char-syntax/raw-entries"
     (with-syntax-table (standard-syntax-table)
       (mapcar (lambda (c) (aref (standard-syntax-table) c))
               '(0 32 34 40 41 91 92 97 #x300c #x4e00))))
(srt "char-syntax/default" (char-table-range (standard-syntax-table) nil))
(srt "char-syntax/err-negative" (char-syntax -1))
(srt "char-syntax/err-symbol" (char-syntax 'a))
(srt "char-syntax/err-too-big" (char-syntax #x400000))
(srt "char-syntax/err-float" (char-syntax 1.0))

;;; string-to-syntax and syntax-class-to-char

(srt "string-to-syntax/classes"
     (mapcar #'string-to-syntax
             '("w" "W" "_" "." " " "-" "(" ")" "\"" "\\" "<" ">" "'" "$"
               "/" "@" "!" "|")))
(srt "string-to-syntax/matching"
     (mapcar #'string-to-syntax '("()" ")(" "(]" ". " "w p" "(»")))
(srt "string-to-syntax/flags"
     (mapcar #'string-to-syntax
             '(". 124b" ". 23" "< 2" "> b" ". 1234pbnc" "w x" "_ p" ". 14n")))
(srt "string-to-syntax/shared"
     (list (eq (string-to-syntax "w") (string-to-syntax "w"))
           (eq (string-to-syntax "w") (aref (standard-syntax-table) ?a))
           (eq (string-to-syntax "()") (string-to-syntax "()"))
           (eq (string-to-syntax ". 1") (string-to-syntax ". 1"))))
(srt "string-to-syntax/err-empty" (string-to-syntax ""))
(srt "string-to-syntax/err-letter" (string-to-syntax "Z"))
(srt "string-to-syntax/err-hash" (string-to-syntax "#"))
(srt "string-to-syntax/err-type" (string-to-syntax 1))
(srt "syntax-class-to-char/all"
     (let ((i 0) out)
       (while (< i 16) (push (syntax-class-to-char i) out) (setq i (1+ i)))
       (concat (nreverse out))))
(srt "syntax-class-to-char/err-low" (syntax-class-to-char -1))
(srt "syntax-class-to-char/err-high" (syntax-class-to-char 16))
(srt "syntax-class-to-char/err-type" (syntax-class-to-char 'w))
(srt "syntax-class/values"
     (list (syntax-class nil) (syntax-class '(2)) (syntax-class '(65537 . 41))
           (syntax-class (string-to-syntax ". 124b"))))

;;; Tables: predicates, construction, inheritance

(srt "syntax-table-p"
     (list (syntax-table-p (standard-syntax-table))
           (syntax-table-p (make-syntax-table))
           (syntax-table-p (make-char-table 'foo))
           (syntax-table-p 1) (syntax-table-p nil)))
(srt "standard-syntax-table/eq"
     (eq (standard-syntax-table) (standard-syntax-table)))
(srt "make-syntax-table/parent"
     (let ((parent (make-syntax-table)))
       (list (eq (char-table-parent (make-syntax-table)) (standard-syntax-table))
             (eq (char-table-parent (make-syntax-table parent)) parent)
             (char-table-subtype (make-syntax-table))
             (char-table-range (make-syntax-table) nil)
             (aref (make-syntax-table) ?a))))
(srt "inherit/unset-chars-follow-standard"
     (let ((table (make-syntax-table)))
       (with-syntax-table table (concat (mapcar #'char-syntax srt-chars)))))
(srt "inherit/parent-edit-visible"
     (let* ((parent (make-syntax-table)) (child (make-syntax-table parent)))
       (modify-syntax-entry ?a "." parent)
       (list (with-syntax-table child (char-syntax ?a))
             (progn (modify-syntax-entry ?a "_" child)
                    (with-syntax-table child (char-syntax ?a)))
             (with-syntax-table parent (char-syntax ?a))
             (with-syntax-table (standard-syntax-table) (char-syntax ?a)))))
(srt "copy-syntax-table/standard"
     (let ((copy (copy-syntax-table)))
       (list (eq copy (standard-syntax-table))
             (eq (char-table-parent copy) (standard-syntax-table))
             (char-table-range copy nil)
             (aref copy ?\() (aref copy #x4e00)
             (progn (modify-syntax-entry ?\( "." copy)
                    (list (aref copy ?\() (aref (standard-syntax-table) ?\())))))
(srt "copy-syntax-table/child"
     (let* ((parent (make-syntax-table))
            (table (make-syntax-table parent)))
       (modify-syntax-entry ?x "." table)
       (let ((copy (copy-syntax-table table)))
         (list (eq (char-table-parent copy) parent) (aref copy ?x)
               (aref copy ?y)))))
(srt "copy-syntax-table/err" (copy-syntax-table 1))

;;; modify-syntax-entry

(srt "modify-syntax-entry/values"
     (let ((table (make-syntax-table)))
       (list (modify-syntax-entry ?a "." table)
             (modify-syntax-entry '(?0 . ?9) "_" table)
             (modify-syntax-entry ?{ "){" table)
             (modify-syntax-entry ?/ ". 124b" table)
             (modify-syntax-entry ?\n "> b" table)
             (modify-syntax-entry #x4e00 "_" table)
             (mapcar (lambda (c) (aref table c)) '(?a ?0 ?5 ?9 ?{ ?/ ?\n #x4e00 ?b))
             (with-syntax-table table
               (concat (mapcar #'char-syntax '(?a ?0 ?5 ?9 ?{ ?/ ?\n #x4e00 ?b)))))))
(srt "modify-syntax-entry/inherit-letter"
     (let ((table (make-syntax-table)))
       (modify-syntax-entry ?a "." table)
       (modify-syntax-entry ?a "@" table)
       (list (aref table ?a) (with-syntax-table table (char-syntax ?a)))))
(srt "modify-syntax-entry/current-table"
     (with-temp-buffer
       (set-syntax-table (make-syntax-table))
       (modify-syntax-entry ?q "w")
       (modify-syntax-entry ?% "." )
       (list (char-syntax ?%) (aref (syntax-table) ?%)
             (aref (standard-syntax-table) ?%))))
(srt "modify-syntax-entry/err-char"
     (modify-syntax-entry -1 "w" (make-syntax-table)))
(srt "modify-syntax-entry/err-range"
     (modify-syntax-entry '(?a . -2) "w" (make-syntax-table)))
(srt "modify-syntax-entry/err-letter"
     (modify-syntax-entry ?a "Z" (make-syntax-table)))
(srt "modify-syntax-entry/err-table"
     ;; The error data holds the table itself; show only its shape.
     (condition-case err
         (modify-syntax-entry ?a "w" (make-char-table 'foo))
       (error (list 'ERR (car err) (car (cdr err))
                    (char-table-subtype (car (cdr (cdr err))))))))
(srt "set-syntax-table/err" (with-temp-buffer (set-syntax-table 1)))

;;; matching-paren, with-syntax-table, syntax-after

(srt "matching-paren"
     (with-syntax-table (standard-syntax-table)
       (mapcar #'matching-paren '(?\( ?\) ?\[ ?\] ?{ ?} ?a ?\" #x300c #xff08 171))))
(srt "matching-paren/err" (matching-paren -1))
(srt "with-syntax-table/value-and-restore"
     (with-temp-buffer
       (let ((table (make-syntax-table)) (before nil))
         (set-syntax-table (standard-syntax-table))
         (setq before (syntax-table))
         (modify-syntax-entry ?a "." table)
         (list (with-syntax-table table (list (char-syntax ?a) (eq (syntax-table) table)))
               (eq (syntax-table) before)
               (catch 'out (with-syntax-table table (throw 'out (char-syntax ?a))))
               (eq (syntax-table) before)
               (condition-case nil (with-syntax-table table (error "x")) (error 'caught))
               (eq (syntax-table) before)))))
(srt "syntax-after"
     (with-temp-buffer
       (set-syntax-table (standard-syntax-table))
       (insert "a(b ;\"日")
       (list (syntax-after 1) (syntax-after 2) (syntax-after 4) (syntax-after 6)
             (syntax-after 7) (syntax-after 0) (syntax-after (point-max))
             (syntax-class (syntax-after 2)))))
(srt "syntax-after/property"
     (with-temp-buffer
       (set-syntax-table (standard-syntax-table))
       (insert "ab")
       (put-text-property 1 2 'syntax-table (string-to-syntax "."))
       (list (let ((parse-sexp-lookup-properties nil)) (syntax-after 1))
             (let ((parse-sexp-lookup-properties t)) (syntax-after 1))
             (let ((parse-sexp-lookup-properties t)) (syntax-after 2)))))

;;; Syntax classes in regexps

(defconst srt-regexps
  '(("\\sw+" "  foo_bar baz")
    ("\\s-+" "foo  \tbar")
    ("\\s_+" "foo_-+bar")
    ("\\s.+" "ab,.;cd")
    ("\\s(\\sw*\\s)" "x (abc) y")
    ("\\Sw+" "abc,.;def")
    ("\\bbar\\b" "foo bar baz")
    ("\\bbar\\b" "foobar bar")
    ("\\Bo" "foo")
    ("\\<b" "a_b b")
    ("o\\>" "foo bar")
    ("\\_<foo-bar\\_>" "x foo-bar y")
    ("\\_<bar" "foo-bar bar")
    ("\\sw+" "«日本語»")
    ("\\s.+" "、。x")
    ("\\`\\(\\sw\\|\\s_\\)*\\'" "unsigned_long")
    ("\\`\\sw*\\'" "abc_def")))

(defun srt-matches ()
  (mapcar (lambda (case)
            (let ((start (string-match (car case) (cadr case))))
              (and start (list start (match-end 0)))))
          srt-regexps))

(srt "string-match/standard"
     (with-syntax-table (standard-syntax-table)
       (let ((case-fold-search nil)) (srt-matches))))
(srt "string-match/standard-fold"
     (with-syntax-table (standard-syntax-table)
       (let ((case-fold-search t)) (srt-matches))))
(srt "string-match/modified"
     (let ((table (make-syntax-table)))
       (modify-syntax-entry ?_ "w" table)
       (modify-syntax-entry ?- "w" table)
       (modify-syntax-entry ?, " " table)
       (with-syntax-table table
         (let ((case-fold-search nil)) (srt-matches)))))
(srt "string-match/edit-between-calls"
     (let ((table (make-syntax-table)))
       (with-syntax-table table
         (let ((case-fold-search nil) out)
           (push (string-match "\\`\\sw+\\'" "a_b") out)
           (modify-syntax-entry ?_ "w" table)
           (push (string-match "\\`\\sw+\\'" "a_b") out)
           (aset table ?_ (string-to-syntax "_"))
           (push (string-match "\\`\\sw+\\'" "a_b") out)
           (nreverse out)))))
(srt "re-search-forward/buffer"
     (with-temp-buffer
       (set-syntax-table (make-syntax-table))
       (modify-syntax-entry ?_ "w")
       (insert "  foo_bar, baz-qux (x)")
       (let ((case-fold-search nil) out)
         (goto-char (point-min))
         (dolist (re '("\\sw+" "\\s-+" "\\_<baz" "\\<qux" "\\s(" "x\\>"
                       "\\bbar" "\\s_+"))
           (push (list re (re-search-forward re nil t) (match-beginning 0))
                 out))
         (nreverse out))))
(srt "re-search-forward/syntax-property"
     (with-temp-buffer
       (set-syntax-table (standard-syntax-table))
       (insert "ab cd")
       (goto-char (point-min))
       (list (re-search-forward "\\sw+" nil t) (match-beginning 0))))

;; End here: the standalone `--load' would otherwise print the last value.
(kill-emacs 0)

;;; syntax-rt-semantics.el ends here
