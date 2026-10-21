;;; gnu-semantics-syntax.el --- GNU 31.1 syntax scanning fixture -*- lexical-binding: t; -*-

;; Generate the golden output with emacs -Q --batch -l FILE.
;; Set every table explicitly except the test of the runtime's buffer default.
(defun gss-row (name thunk)
  (princ (format "%s => %S\n" name
                 (condition-case err (funcall thunk)
                   (error (cons 'ERR err))))))

(defmacro gss (name &rest body)
  `(gss-row ,name (lambda () ,@body)))

(defmacro gss-buffer (text &rest body)
  `(with-temp-buffer
     (set-syntax-table (copy-syntax-table (standard-syntax-table)))
     (insert ,text)
     ,@body))

(defun gss-skip (direction syntax &optional limit)
  (list (funcall direction syntax limit) (point)))

(gss "forward/classes"
     (gss-buffer "abc_+- \t\n()'\"\\!"
       (goto-char 1)
       (list (gss-skip 'skip-syntax-forward "W_")
             (gss-skip 'skip-syntax-forward " -")
             (gss-skip 'skip-syntax-forward "()")
             (gss-skip 'skip-syntax-forward ".\"\\"))))
(gss "backward/classes"
     (gss-buffer "abc_+- \t\n()'\"\\!"
       (list (gss-skip 'skip-syntax-backward ".\"\\")
             (gss-skip 'skip-syntax-backward "()")
             (gss-skip 'skip-syntax-backward " -")
             (gss-skip 'skip-syntax-backward "w_"))))
(gss "classes/all"
     (gss-buffer "abcdefghijklmnop"
       (let ((specs '(" " "." "w" "_" "(x" ")x" "'" "\"" "$" "\\" "/" "<" ">" "@" "!" "|"))
             (char ?a))
         (dolist (spec specs)
           (modify-syntax-entry char spec)
           (setq char (1+ char))))
       (goto-char 1)
       (gss-skip 'skip-syntax-forward " .w_()'\"$\\/<>@!|")))
(gss "forward/complements"
     (gss-buffer "abc +def"
       (goto-char 1)
       (list (gss-skip 'skip-syntax-forward "^ ")
             (gss-skip 'skip-syntax-forward "^w")
             (gss-skip 'skip-syntax-forward "^"))))
(gss "backward/complements"
     (gss-buffer "abc +def"
       (list (gss-skip 'skip-syntax-backward "^ ")
             (gss-skip 'skip-syntax-backward "^w")
             (gss-skip 'skip-syntax-backward "^"))))
(gss "spec/empty-unknown"
     (gss-buffer " a^b"
       (goto-char 1)
       (list (gss-skip 'skip-syntax-forward "")
             (gss-skip 'skip-syntax-forward "x")
             (gss-skip 'skip-syntax-forward "x ")
             (gss-skip 'skip-syntax-forward "w^")
             (gss-skip 'skip-syntax-forward "^x"))))
(gss "spec/multibyte-conversion"
     (gss-buffer "ab cd"
       (goto-char 1)
       (list (gss-skip 'skip-syntax-forward (string #x177))
             (gss-skip 'skip-syntax-forward (string #x120))
             (gss-skip 'skip-syntax-forward (string #x15e)))))
(gss "limits/forward"
     (gss-buffer "abcdef"
       (goto-char 2)
       (list (gss-skip 'skip-syntax-forward "w" 4)
             (gss-skip 'skip-syntax-forward "w" 3)
             (gss-skip 'skip-syntax-forward "w" -100)
             (gss-skip 'skip-syntax-forward "w" 100)
             (gss-skip 'skip-syntax-forward "w"))))
(gss "limits/backward"
     (gss-buffer "abcdef"
       (goto-char 6)
       (list (gss-skip 'skip-syntax-backward "w" 4)
             (gss-skip 'skip-syntax-backward "w" 5)
             (gss-skip 'skip-syntax-backward "w" 100)
             (gss-skip 'skip-syntax-backward "w" -100)
             (gss-skip 'skip-syntax-backward "w"))))
(gss "limits/markers"
     (gss-buffer "abcdef"
       (let ((limit (copy-marker 4)))
         (goto-char 1)
         (list (gss-skip 'skip-syntax-forward "w" limit)
               (progn (set-marker limit 2)
                      (gss-skip 'skip-syntax-backward "w" limit))))))
(gss "limits/narrowing"
     (gss-buffer "abcdefgh"
       (narrow-to-region 3 7)
       (goto-char 3)
       (list (gss-skip 'skip-syntax-forward "w" 100)
             (gss-skip 'skip-syntax-backward "w" -100))))
(gss "multibyte"
     (gss-buffer "漢字αβ abc"
       (goto-char 1)
       (list (gss-skip 'skip-syntax-forward "w")
             (gss-skip 'skip-syntax-forward "^")
             (gss-skip 'skip-syntax-backward "w")
             (gss-skip 'skip-syntax-backward "^"))))
(gss "unibyte"
     (gss-buffer ""
       (set-buffer-multibyte nil)
       (insert (unibyte-string 97 128 255 32 98))
       (goto-char 1)
       (list (gss-skip 'skip-syntax-forward "^")
             (gss-skip 'skip-syntax-backward "^"))))
(gss "properties/classes"
     (gss-buffer "aabbcc"
       (put-text-property 3 5 'syntax-table (string-to-syntax "."))
       (goto-char 1)
       (let ((parse-sexp-lookup-properties t))
         (list (gss-skip 'skip-syntax-forward "w")
               (gss-skip 'skip-syntax-forward ".")
               (gss-skip 'skip-syntax-forward "w")
               (gss-skip 'skip-syntax-backward "w")
               (gss-skip 'skip-syntax-backward ".")
               (gss-skip 'skip-syntax-backward "w")))))
(gss "properties/table"
     (gss-buffer "aa@aa"
       (let ((table (copy-syntax-table (syntax-table)))
             (parse-sexp-lookup-properties t))
         (modify-syntax-entry ?@ "w" table)
         (put-text-property 3 4 'syntax-table table)
         (goto-char 1)
         (list (gss-skip 'skip-syntax-forward "w")
               (gss-skip 'skip-syntax-backward "w")))))
(gss "properties/fences"
     (gss-buffer "aabb"
       (put-text-property 1 3 'syntax-table (string-to-syntax "!"))
       (put-text-property 3 5 'syntax-table (string-to-syntax "|"))
       (goto-char 1)
       (let ((parse-sexp-lookup-properties t))
         (list (gss-skip 'skip-syntax-forward "!")
               (gss-skip 'skip-syntax-forward "|")
               (gss-skip 'skip-syntax-backward "|")
               (gss-skip 'skip-syntax-backward "!")))))
(gss "properties/ignored"
     (gss-buffer "aaaa"
       (put-text-property 1 5 'syntax-table (string-to-syntax "."))
       (goto-char 1)
       (let ((parse-sexp-lookup-properties nil))
         (gss-skip 'skip-syntax-forward "w"))))
(gss "buffer-default"
     (let ((table (copy-syntax-table (standard-syntax-table))))
       (modify-syntax-entry ?@ "w" table)
       (if (boundp 'nelisp--buffer-defaults-syntax-table)
           (let ((nelisp--buffer-defaults-syntax-table table))
             (with-temp-buffer (insert "@@") (goto-char 1)
               (gss-skip 'skip-syntax-forward "w")))
         (with-temp-buffer (set-syntax-table table) (insert "@@")
           (goto-char 1) (gss-skip 'skip-syntax-forward "w")))))
(gss "prefix/class-and-flag"
     (gss-buffer "x'`#"
       (modify-syntax-entry ?' "'")
       (modify-syntax-entry ?` "'")
       (modify-syntax-entry ?# "w p")
       (list (backward-prefix-chars) (point))))
(gss "prefix/non-prefix"
     (gss-buffer "'x"
       (modify-syntax-entry ?' "'")
       (list (backward-prefix-chars) (point))))
(gss "prefix/empty"
     (gss-buffer "" (list (backward-prefix-chars) (point))))
(gss "prefix/quoted"
     (mapcar (lambda (text)
               (gss-buffer text
                 (modify-syntax-entry ?' "'")
                 (list (backward-prefix-chars) (point))))
             '("\\'" "\\\\'" "\\\\\\'" "\\''" "\\\\''")))
(gss "prefix/character-quote"
     (gss-buffer "/'"
       (modify-syntax-entry ?/ "/")
       (modify-syntax-entry ?' "'")
       (list (backward-prefix-chars) (point))))
(gss "prefix/narrowing"
     (gss-buffer "\\''"
       (modify-syntax-entry ?' "'")
       (narrow-to-region 2 4)
       (list (backward-prefix-chars) (point))))
(gss "prefix/properties"
     (gss-buffer "x@@"
       (put-text-property 2 4 'syntax-table (string-to-syntax ". p"))
       (let ((parse-sexp-lookup-properties t))
         (list (backward-prefix-chars) (point)))))
(gss "prefix/properties-quote"
     (gss-buffer "x@'"
       (modify-syntax-entry ?' "'")
       (put-text-property 2 3 'syntax-table (string-to-syntax "\\"))
       (let ((parse-sexp-lookup-properties t))
         (list (backward-prefix-chars) (point)))))
(gss "prefix/properties-ignored"
     (gss-buffer "@@"
       (put-text-property 1 3 'syntax-table (string-to-syntax ". p"))
       (let ((parse-sexp-lookup-properties nil))
         (list (backward-prefix-chars) (point)))))
(gss "types/syntax" (skip-syntax-forward 7))
(gss "types/backward-syntax" (skip-syntax-backward nil))
(gss "types/syntax-before-limit" (skip-syntax-forward 7 'bad))
(gss "types/limit-symbol" (skip-syntax-forward "w" 'bad))
(gss "types/limit-float" (skip-syntax-backward "w" 1.0))
(gss "types/limit-bignum" (skip-syntax-forward "w" #x100000000000000000000000))
(gss "types/unbound-marker" (skip-syntax-forward "w" (make-marker)))

;; Avoid the standalone --load driver's printing of the final form's value.
(kill-emacs 0)

;;; gnu-semantics-syntax.el ends here
