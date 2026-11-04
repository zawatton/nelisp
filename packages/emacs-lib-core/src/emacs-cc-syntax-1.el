;;; emacs-cc-syntax-1.el --- Syntax C primitives -*- lexical-binding: t; -*-

(defun emacs-cc-syntax-1--class-name (n)
  (nth n '("whitespace" "punctuation" "word" "symbol" "open" "close"
           "quote" "string" "paired delimiter" "escape" "character quote"
           "comment-start" "comment-end" "inherit" "prefix" "postfix")))

(unless (fboundp 'syntax-class-to-char)
  (defun syntax-class-to-char (syntax)
    "Return the syntax char of CLASS, described by an integer.
For example, if SYNTAX is word constituent (the integer 2), the
character `w' (119) is returned."
    (unless (integerp syntax) (signal 'wrong-type-argument (list 'fixnump syntax)))
    (unless (<= 0 syntax 15) (signal 'args-out-of-range (list 15 syntax)))
    (nth syntax '(32 46 119 95 40 41 39 34 36 92 47 60 62 64 33 124))))

(unless (fboundp 'matching-paren)
  (defun matching-paren (character)
    "Return the matching parenthesis of CHARACTER, or nil if none."
    (unless (and (integerp character) (<= 0 character 1114111))
      (signal 'wrong-type-argument (list 'characterp character)))
    (cond ((= character 40) 41) ((= character 41) 40)
          ((= character 91) 93) ((= character 93) 91)
          ((= character 123) 125) ((= character 125) 123))))

(unless (fboundp 'internal-describe-syntax-value)
  (defun internal-describe-syntax-value (syntax)
    "Insert a description of the internal syntax description SYNTAX at point."
    (cond
     ((null syntax) (insert "default"))
     ((and (consp syntax) (integerp (car syntax)) (<= 0 (car syntax) 15))
      (let* ((class (car syntax)) (match (cdr syntax))
             (ch (syntax-class-to-char class))
             (name (emacs-cc-syntax-1--class-name class)))
        (insert (char-to-string ch))
        (when (integerp match) (insert (char-to-string match)))
        (insert (if (integerp match) "\twhich means: "
                  " \twhich means: ") name)
        (when (integerp match)
          (insert ", matches " (char-to-string match)))))
     (t (insert "invalid")))))

(unless (fboundp 'backward-prefix-chars)
  (defun backward-prefix-chars ()
    "Move point backward over any number of chars with prefix syntax.
This includes chars with expression prefix syntax class (`=') and those with
the prefix syntax flag (`p')."
    (while (and (> (point) (point-min))
                (let* ((ch (char-before)) (syn (char-syntax ch)))
                  (or (eq syn 33)
                      (and (fboundp 'syntax-after)
                           (let ((desc (syntax-after (1- (point)))))
                             (and (integerp desc)
                                  (/= 0 (logand desc 1048576)))))))
      (backward-char)))))

(provide 'emacs-cc-syntax-1)
