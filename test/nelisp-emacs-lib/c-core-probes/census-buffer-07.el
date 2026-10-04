;;; census-buffer-07.el --- canonical probes  -*- lexical-binding: t; -*-

(skip-syntax-backward
 (with-temp-buffer (insert "abc  ") (list (skip-syntax-backward " ") (point)))
 (with-temp-buffer (insert "abcd") (list (skip-syntax-backward "w" 3) (point)))
 (condition-case e (skip-syntax-backward 7) (error e)))

(skip-syntax-forward
 (with-temp-buffer (insert "abc  ") (goto-char 1) (list (skip-syntax-forward "w") (point)))
 (with-temp-buffer (insert "abcd") (goto-char 1) (list (skip-syntax-forward "w" 3) (point)))
 (condition-case e (skip-syntax-forward 7) (error e)))

(standard-case-table
 (let ((table (standard-case-table))) (list (char-table-p table) (aref table ?A) (aref table ?a)))
 (eq (standard-case-table) (standard-case-table))
 (condition-case e (standard-case-table 1) (error (car e))))

(standard-category-table
 (char-table-p (standard-category-table))
 (eq (standard-category-table) (standard-category-table))
 (condition-case e (standard-category-table 1) (error (car e))))

(standard-syntax-table
 (let ((table (standard-syntax-table))) (list (char-table-p table) (aref table ?a) (aref table ?\s)))
 (eq (standard-syntax-table) (standard-syntax-table))
 (condition-case e (standard-syntax-table 1) (error (car e))))

(string-to-syntax
 (string-to-syntax "w")
 (string-to-syntax "()")
 (condition-case e (string-to-syntax 7) (error e)))

(subst-char-in-region
 (with-temp-buffer (insert "abaca") (subst-char-in-region 1 6 ?a ?x) (buffer-string))
 (with-temp-buffer (insert "aaa") (subst-char-in-region 2 2 ?a ?x) (buffer-string))
 (with-temp-buffer (condition-case e (subst-char-in-region 'bad 1 ?a ?x) (error e))))

;; Arity checks cannot suspend the process or invoke hooks.
(suspend-emacs
 (condition-case e (suspend-emacs nil nil) (error (car e)))
 (condition-case e (suspend-emacs nil nil nil) (error (car e))))

(syntax-table
 (with-temp-buffer (let ((table (syntax-table))) (list (syntax-table-p table) (aref table ?a))))
 (with-temp-buffer (let ((table (make-syntax-table))) (set-syntax-table table) (eq (syntax-table) table)))
 (condition-case e (syntax-table 1) (error (car e))))

(syntax-table-p
 (syntax-table-p (make-syntax-table))
 (list (syntax-table-p nil) (syntax-table-p [1 2]) (syntax-table-p (make-char-table 'category-table))))

(test-completion
 (test-completion "alpha" '("alpha" "alpine" "beta"))
 (list (test-completion "al" '("alpha" "alpine")) (test-completion "" '("" "alpha")))
 (condition-case e (test-completion 7 '("alpha")) (error e)))

(text-properties-at
 (let ((s (copy-sequence "abc"))) (put-text-property 0 2 'probe 'yes s) (text-properties-at 1 s))
 (text-properties-at 0 "abc")
 (condition-case e (text-properties-at 4 "abc") (error e)))

(text-property-any
 (let ((s (copy-sequence "abc"))) (put-text-property 1 3 'probe 'yes s) (text-property-any 0 3 'probe 'yes s))
 (text-property-any 0 3 'probe 'yes "abc")
 (condition-case e (text-property-any -1 3 'probe nil "abc") (error e)))

(text-property-not-all
 (let ((s (copy-sequence "abc"))) (put-text-property 0 2 'probe 'yes s) (text-property-not-all 0 3 'probe 'yes s))
 (text-property-not-all 0 3 'probe nil "abc")
 (condition-case e (text-property-not-all -1 3 'probe nil "abc") (error e)))

(this-command-keys
 (this-command-keys)
 (condition-case e (this-command-keys nil) (error (car e))))

(this-command-keys-vector
 (this-command-keys-vector)
 (condition-case e (this-command-keys-vector nil) (error (car e))))

(this-single-command-keys
 (this-single-command-keys)
 (condition-case e (this-single-command-keys nil) (error (car e))))

(this-single-command-raw-keys
 (this-single-command-raw-keys)
 (condition-case e (this-single-command-raw-keys nil) (error (car e))))

;; Arity checks cannot exit the evaluation or enter a recursive edit.
(top-level
 (condition-case e (top-level nil) (error (car e)))
 (condition-case e (top-level nil nil) (error (car e))))

(try-completion
 (try-completion "al" '("alpha" "alpine" "beta"))
 (list (try-completion "alpha" '("alpha" "alpine")) (try-completion "z" '("alpha")))
 (condition-case e (try-completion 7 '("alpha")) (error e)))

(undo-boundary
 (with-temp-buffer (buffer-enable-undo) (insert "abc") (undo-boundary) (null (car buffer-undo-list)))
 (with-temp-buffer (buffer-enable-undo) (undo-boundary) (null buffer-undo-list))
 (condition-case e (undo-boundary nil) (error (car e))))

(upcase
 (upcase "Hello abc")
 (list (upcase ?a) (upcase ""))
 (condition-case e (upcase '(a)) (error e)))

(upcase-initials
 (upcase-initials "hello WORLD two-words")
 (list (upcase-initials ?a) (upcase-initials ""))
 (condition-case e (upcase-initials '(a)) (error e)))

(upcase-region
 (with-temp-buffer (insert "aBc de") (upcase-region 1 4) (buffer-string))
 (with-temp-buffer (insert "abc") (upcase-region 2 2) (buffer-string))
 (with-temp-buffer (condition-case e (upcase-region 'bad 1) (error e))))

(upcase-word
 (with-temp-buffer (insert "one two") (goto-char 1) (upcase-word 1) (list (buffer-string) (point)))
 (with-temp-buffer (insert "one two") (upcase-word -1) (list (buffer-string) (point)))
 (with-temp-buffer (condition-case e (upcase-word 'bad) (error e))))

(use-global-map
 (let ((old (current-global-map)) (map (make-sparse-keymap))) (unwind-protect (progn (use-global-map map) (eq (current-global-map) map)) (use-global-map old)))
 (let ((old (current-global-map)) (map (make-keymap))) (unwind-protect (progn (use-global-map map) (eq (current-global-map) map)) (use-global-map old)))
 (condition-case e (use-global-map 7) (error e)))

(use-local-map
 (with-temp-buffer (let ((map (make-sparse-keymap))) (use-local-map map) (eq (current-local-map) map)))
 (with-temp-buffer (use-local-map nil) (current-local-map))
 (with-temp-buffer (condition-case e (use-local-map 7) (error e))))

(where-is-internal
 (let ((map (make-sparse-keymap))) (define-key map [97] 'probe-command) (where-is-internal 'probe-command (list map)))
 (let ((map (make-sparse-keymap))) (define-key map [97] 'probe-command) (where-is-internal 'probe-command (list map) t))
 (where-is-internal 'probe-missing-command (list (make-sparse-keymap))))

(widen
 (with-temp-buffer (insert "abcdef") (narrow-to-region 2 4) (widen) (list (point-min) (point-max) (buffer-string)))
 (with-temp-buffer (widen) (list (point-min) (point-max)))
 (condition-case e (widen nil) (error (car e))))

;; Arity checks cannot prompt for keyboard input.
(yes-or-no-p
 (condition-case e (yes-or-no-p) (error (car e)))
 (condition-case e (yes-or-no-p "unused" nil) (error (car e))))

(zlib-available-p
 (let ((value (zlib-available-p))) (or (eq value t) (null value)))
 (eq (zlib-available-p) (zlib-available-p))
 (condition-case e (zlib-available-p nil) (error (car e))))
