;;; standalone-casetab-behaviour-check.el --- GNU case operations fixture -*- lexical-binding: t -*-

;; The oracle preloads case-table; the standalone invocation loads only this
;; fixture.  No implementation source is loaded into the tested executable.
(defvar casetab--count 0)
(defun casetab--row (name thunk)
  (setq casetab--count (1+ casetab--count))
  (prin1 (list 'CASETAB 'behaviour name
               (condition-case condition
                   (list 'ok (funcall thunk))
                 (error (cons 'error condition)))))
  (terpri))
(defun casetab--in-table (thunk)
  (with-temp-buffer
    (let ((table (make-char-table 'case-table nil)))
      (set-case-syntax-pair 65 955 table)
      (set-case-syntax-pair 66 98 table)
      (set-case-table table)
      (funcall thunk))))
(defun casetab--region (operation)
  (casetab--in-table
   (lambda ()
     (insert "Aλ bB Aλ")
     (funcall operation 2 7)
     (buffer-string))))
(defun casetab--search (operation pattern fold backwards)
  (casetab--in-table
   (lambda ()
     (insert "x λb x AB x")
     (goto-char (if backwards (point-max) (point-min)))
     (let ((case-fold-search fold))
       (let ((found (funcall operation pattern nil t)))
         (list found (point)
               (and found (match-beginning 0))
               (and found (match-end 0))))))))

(casetab--row "standard-unicode"
 (lambda () (list (upcase 955) (downcase 923)
                  (upcase "aλ") (downcase "AΛ"))))
(casetab--row "custom-chars"
 (lambda () (casetab--in-table
             (lambda () (list (upcase 65) (upcase 955)
                              (downcase 65) (downcase 955)
                              (upcase 98) (downcase 66))))))
(casetab--row "custom-upcase"
 (lambda () (casetab--in-table (lambda () (upcase "Aλ bB Aλ")))))
(casetab--row "custom-downcase"
 (lambda () (casetab--in-table (lambda () (downcase "Aλ bB Aλ")))))
(casetab--row "custom-capitalize"
 (lambda () (casetab--in-table (lambda () (capitalize "λA bB λA")))))
(casetab--row "custom-initials"
 (lambda () (casetab--in-table (lambda () (upcase-initials "λA bB λA")))))
(casetab--row "capitalize-character"
 (lambda () (casetab--in-table (lambda () (capitalize 955)))))
(casetab--row "initials-character"
 (lambda () (casetab--in-table (lambda () (upcase-initials 955)))))
(casetab--row "upcase-region" (lambda () (casetab--region 'upcase-region)))
(casetab--row "downcase-region" (lambda () (casetab--region 'downcase-region)))
(casetab--row "capitalize-region" (lambda () (casetab--region 'capitalize-region)))
(casetab--row "initials-region" (lambda () (casetab--region 'upcase-initials-region)))
(casetab--row "char-equal-fold"
 (lambda () (casetab--in-table
             (lambda () (let ((case-fold-search t))
                          (list (char-equal 65 955) (char-equal 955 65)
                                (char-equal 65 97) (char-equal 66 98)))))))
(casetab--row "char-equal-sensitive"
 (lambda () (casetab--in-table
             (lambda () (let ((case-fold-search nil))
                          (list (char-equal 65 955) (char-equal 955 65)
                                (char-equal 65 65) (char-equal 66 98)))))))
(casetab--row "literal-forward-fold"
 (lambda () (casetab--search 'search-forward "Ab" t nil)))
(casetab--row "literal-forward-sensitive"
 (lambda () (casetab--search 'search-forward "Ab" nil nil)))
(casetab--row "literal-backward-fold"
 (lambda () (casetab--search 'search-backward "λb" t t)))
(casetab--row "literal-backward-sensitive"
 (lambda () (casetab--search 'search-backward "λb" nil t)))
(casetab--row "regexp-forward-fold"
 (lambda () (casetab--search 're-search-forward "Ab" t nil)))
(casetab--row "regexp-forward-sensitive"
 (lambda () (casetab--search 're-search-forward "Ab" nil nil)))
(casetab--row "regexp-backward-fold"
 (lambda () (casetab--search 're-search-backward "λb" t t)))
(casetab--row "regexp-class-fold"
 (lambda () (casetab--search 're-search-forward "[A][B]" t nil)))
(casetab--row "string-match-fold"
 (lambda () (casetab--in-table
             (lambda () (let ((case-fold-search t))
                          (let ((found (string-match "Ab" "x λb x")))
                            (list found (match-beginning 0) (match-end 0))))))))
(casetab--row "string-match-sensitive"
 (lambda () (casetab--in-table
             (lambda () (let ((case-fold-search nil))
                          (string-match "Ab" "x λb x"))))))
(casetab--row "string-match-custom-class"
 (lambda () (casetab--in-table
             (lambda () (let ((case-fold-search t))
                          (string-match "[A][B]" "x λb x"))))))
(casetab--row "table-is-buffer-local"
 (lambda ()
   (let ((old-down (downcase 65)) (old-up (upcase 955)))
     (list (casetab--in-table (lambda () (list (downcase 65) (upcase 955))))
           (= old-down (downcase 65)) (= old-up (upcase 955))))))
(casetab--row "conversion-input-unchanged"
 (lambda () (casetab--in-table
             (lambda () (let ((input (copy-sequence "Aλ bB")))
                          (list (downcase input) input))))))
(casetab--row "upcase-error" (lambda () (upcase nil)))
(casetab--row "downcase-error" (lambda () (downcase '(a))))
(casetab--row "char-equal-error" (lambda () (char-equal nil 65)))

(casetab--row "unicode-full-uppercase"
 (lambda () (list (upcase "ßﬃ") (upcase 223) (upcase 64259))))
(casetab--row "unicode-full-title"
 (lambda () (list (capitalize "ßﬃ") (upcase-initials "ßﬃ ǆ"))))
(casetab--row "unicode-title-digraph"
 (lambda () (list (capitalize "ǆ ǉ ǌ") (upcase-initials "ǆ ǉ ǌ")
                  (capitalize 454) (upcase 454) (downcase 453))))
(casetab--row "custom-title-overrides-simple-upcase"
 (lambda () (casetab--in-table
             (lambda () (list (upcase 955) (capitalize 955) (upcase-initials 955)
                              (capitalize "λA") (upcase-initials "λA"))))))
(casetab--row "custom-full-versus-simple"
 (lambda () (casetab--in-table
             (lambda () (list (upcase "ß") (upcase 223)
                              (capitalize "ß") (capitalize 223))))))
(casetab--row "unibyte-string-numeric-and-equality"
 (lambda ()
   (with-temp-buffer
     (set-buffer-multibyte nil)
     (let ((table (copy-case-table (standard-case-table))))
       (set-case-syntax-pair 65 955 table)
       (set-case-table table)
       (let ((converted (downcase (unibyte-string 65))))
         (list (downcase 65) converted (multibyte-string-p converted)
               (char-equal 65 955) (char-equal 955 65) (upcase 955)))))))
(casetab--row "multibyte-string-numeric-and-equality"
 (lambda () (casetab--in-table
             (lambda ()
               (let ((converted (downcase (string-as-multibyte (unibyte-string 65)))))
                 (list (downcase 65) converted (multibyte-string-p converted)
                       (char-equal 65 955)))))))
(casetab--row "region-expansion-point-markers-properties"
 (lambda ()
   (with-temp-buffer
     (insert "xß y")
     (let ((begin (copy-marker 2)) (end (copy-marker 3))
           (after (copy-marker 5)))
       (put-text-property 2 3 'casetab-property 'kept)
       (goto-char 4)
       (upcase-region 2 3)
       (list (buffer-substring-no-properties (point-min) (point-max))
             (point) (marker-position begin) (marker-position end)
             (marker-position after)
             (get-text-property 2 'casetab-property)
             (get-text-property 3 'casetab-property)
             (get-text-property 4 'casetab-property))))))
(casetab--row "region-expansion-narrowing"
 (lambda ()
   (with-temp-buffer
     (insert "xß yZ")
     (let ((inside (copy-marker 3)))
       (narrow-to-region 2 5)
       (goto-char 4)
       (upcase-region (point-min) (point-max))
       (let ((state (list (buffer-substring-no-properties (point-min) (point-max))
                          (point) (point-min) (point-max) (marker-position inside))))
         (widen)
         (append state (list (buffer-substring-no-properties (point-min) (point-max)))))))))
(casetab--row "unibyte-region-custom-fallback"
 (lambda ()
   (with-temp-buffer
     (set-buffer-multibyte nil)
     (let ((table (copy-case-table (standard-case-table))))
       (set-case-syntax-pair 65 955 table)
       (set-case-table table)
       (insert (unibyte-string 65))
       (downcase-region (point-min) (point-max))
       (list (append (buffer-substring-no-properties (point-min) (point-max)) nil)
             enable-multibyte-characters (point))))))
(casetab--row "noncontiguous-region-bounds"
 (lambda ()
   (with-temp-buffer
     (insert "ab cd ef")
     (let* ((called nil)
           (region-extract-function (lambda (operation)
                                      (setq called operation)
                                      '((1 . 3) (7 . 9)))))
       (list (upcase-region 99 999 t) called
             (buffer-substring-no-properties (point-min) (point-max)))))))
(casetab--row "region-prefix-syntax"
 (lambda ()
   (with-temp-buffer
     (modify-syntax-entry 39 "w p")
     (insert "'FOO 'bAR")
     (let ((as-string (capitalize "'FOO 'bAR")))
       (capitalize-region (point-min) (point-max))
       (list as-string (buffer-substring-no-properties (point-min) (point-max)))))))
(casetab--row "region-nochange-modified-state"
 (lambda ()
   (with-temp-buffer
     (insert "abc")
     (set-buffer-modified-p nil)
     (downcase-region 1 4)
     (let ((nonempty (buffer-modified-p)))
       (set-buffer-modified-p nil)
       (downcase-region 2 2)
       (list nonempty (buffer-modified-p))))))

(defun casetab--region-ticks (text operation begin end &optional table)
  (with-temp-buffer
    (when table (set-case-table table))
    (insert text)
    (set-buffer-modified-p nil)
    (let ((tick (buffer-modified-tick)) (chars (buffer-chars-modified-tick)))
      (funcall operation begin end)
      (list (buffer-modified-p) (- (buffer-modified-tick) tick)
            (- (buffer-chars-modified-tick) chars)))))
(casetab--row "region-modified-tick-deltas"
 (lambda ()
   (list (casetab--region-ticks "abc" 'downcase-region 1 4)
         (casetab--region-ticks "abc" 'downcase-region 1 1)
         (casetab--region-ticks "abc" 'upcase-region 1 4)
         (casetab--region-ticks "ßß" 'upcase-region 1 3)
         (let ((table (make-char-table 'case-table nil)))
           (set-case-syntax-pair 65 955 table)
           (casetab--region-ticks "λa" 'upcase-region 1 3 table)))))
(casetab--row "string-properties-by-byte-representation"
 (lambda ()
   (let ((changed (unibyte-string 65)) (unchanged (unibyte-string 97))
         (multibyte (string-as-multibyte (unibyte-string 65))))
     (dolist (input (list changed unchanged multibyte))
       (put-text-property 0 1 'casetab-property 'kept input))
     (mapcar (lambda (input)
               (let ((result (downcase input)))
                 (list (append result nil) (multibyte-string-p result)
                       (get-text-property 0 'casetab-property result)
                       (get-text-property 0 'casetab-property input))))
             (list changed unchanged multibyte)))))
(casetab--row "detached-region-marker-errors"
 (lambda ()
   (with-temp-buffer
     (insert "abc")
     (list (condition-case condition
               (upcase-region (make-marker) 2)
             (error condition))
           (condition-case condition
               (upcase-region 1 (make-marker))
             (error condition))))))

(princ (format "CASETAB-DONE behaviour %d\n" casetab--count))
(kill-emacs 0)
